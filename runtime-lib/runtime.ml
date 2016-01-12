module Test_result = struct
  type t = Success | Failure | Error

  let to_exit_code = function
    | Success -> 0
    | Failure -> 2
    | Error   -> 1
  ;;

  let to_string = function
    | Success -> "success"
    | Failure -> "failure"
    | Error   -> "error"
  ;;

  let combine t1 t2 =
    match t1, t2 with
    | Success, Success        -> Success
    | Error  , _ | _, Error   -> Error
    | Failure, _ | _, Failure -> Failure
  ;;

  let combine_all ts = List.fold_left combine Success ts

  let global_t = ref Success
  let exit () = exit (to_exit_code !global_t)
  let record t = global_t := combine !global_t t
end

let parse_argv argv l f msg =
  try
    Arg.parse_argv argv l f msg
  with
  | Arg.Bad msg -> Printf.eprintf "%s" msg; exit 1
  | Arg.Help msg -> Printf.printf "%s" msg; exit 0
;;

type descr = string
let test_modules_ran = ref 0
let test_modules_failed = ref 0
let tests_ran = ref 0
let tests_failed = ref 0
let dynamic_lib : string option ref = ref None
type filename = string
type line_number = int
type start_pos = int
type end_pos = int
let action : [
| `Ignore
| `Run_lib of string * (filename * line_number option * bool ref) list
| `Collect of (unit -> unit) list ref
] ref = ref `Ignore
let module_descr = ref []
let verbose = ref false
let strict = ref false
let show_counts = ref false
let list_test_names = ref false
let delayed_errors = ref []
let stop_on_error = ref false

let log = ref None

let time_sec = ref 0.

let use_color = ref true

let displayed_descr descr filename line start_pos end_pos =
  Printf.sprintf "File %S, line %d, characters %d-%d%s"
    filename line start_pos end_pos descr
let parse_descr str =
  try Some (Scanf.sscanf str " File %S , line %d , characters %d - %d %!"
              (fun file line _start_pos _end_pos -> file, Some line))
  with _ ->
    try Some (Scanf.sscanf str " File %S , line %d %!" (fun file line -> file, Some line))
    with _ ->
      try Some (Scanf.sscanf str " File %S %!" (fun file -> file, None))
      with _ -> None

let indent ~by str =
  let len = String.length str in
  let buf = Buffer.create (len * 2) in
  let indentation = String.make by ' ' in
  Buffer.add_string buf indentation;
  for i = 0 to len - 1; do
    Buffer.add_char buf str.[i];
    if str.[i] = '\n' && i <> len - 1 then Buffer.add_string buf indentation
  done;
  Buffer.contents buf

let backtrace_indented ~by =
  let str = Printexc.get_backtrace () in
  indent str ~by

let () =
  match Array.to_list Sys.argv with
  | name :: "inline-test-runner" :: lib :: rest -> begin
    (* when we see this argument, we switch to test mode *)
    let tests = ref [] in
    parse_argv (Array.of_list (name :: rest)) (Arg.align [
      "-list-test-names", Arg.Unit (fun () -> list_test_names := true; verbose := true),
        " Do not run tests but show what would have been run";
      "-verbose", Arg.Set verbose, " Show the tests as they run";
      "-stop-on-error", Arg.Set stop_on_error, " Run tests only up to the first error";
      "-strict", Arg.Set strict, " End with an error if no tests were run";
      "-show-counts", Arg.Set show_counts, " Show the number of tests ran";
      "-log", Arg.Unit (fun () ->
        (try Sys.remove "inline_tests.log" with _ -> ());
        log := Some (open_out "inline_tests.log")
      ), " Log the tests run in inline_tests.log";
      "-only-test", Arg.String (fun s ->
        let filename, index =
          match parse_descr s with
          | Some (file, index) -> file, index
          | None ->
            if String.contains s ':' then
              let i = String.index s ':' in
              let filename = String.sub s 0 i in
              let index_string = String.sub s (i + 1) (String.length s - i - 1) in
              let index =
                try int_of_string index_string
                with Failure _ ->
                  Printf.eprintf
                    "Argument %s doesn't fit the format filename[:line_number]\n%!" s;
                  exit 1
              in
              filename, Some index
          else
            s, None
        in
        tests := (filename, index, ref false) :: !tests
      ), "location Run only the tests specified by all the -only-test options.
                      Locations can be one of these forms:
                      - file.ml
                      - file.ml:line_number
                      - File \"file.ml\"
                      - File \"file.ml\", line 23
                      - File \"file.ml\", line 23, characters 2-3";
      "-no-color", Arg.Clear use_color, " Summarize tests using color";
    ]) (fun anon ->
      Printf.eprintf "%s: unexpected anonymous argument %s\n%!" name anon;
      exit 1
    ) (Printf.sprintf "%s %s %s [args]" name "inline-test-runner" lib);
    action := `Run_lib (lib, !tests)
    end
  | _ ->
    ()

let testing =
  match !action with
  | `Run_lib _ -> true
  | `Ignore -> false
  | `Collect _ -> assert false

let time f =
  let before_sec = Sys.time () in
  let res =
    try f ()
    with e ->
      time_sec := Sys.time () -. before_sec;
      raise e
  in
  time_sec := Sys.time () -. before_sec;
  res

let with_descr (descr : descr) f =
  let prev = !module_descr in
  module_descr := descr :: prev;
  try
    time f;
    module_descr := prev;
  with e ->
    module_descr := prev;
    raise e

let string_of_module_descr () =
  String.concat "" (
    List.map (fun s -> "  in TES" ^ "T_MODULE at " ^ String.uncapitalize s ^ "\n")
      !module_descr
  )

let position_match def_filename def_line_number l =
  List.exists (fun (filename, line_number_opt, used) ->
    let position_start =
      String.length def_filename - String.length filename in
    let found =
      position_start >= 0 &&
        let end_of_def_filename =
          String.sub def_filename
            position_start
            (String.length filename) in
        end_of_def_filename = filename
        && (position_start = 0 || def_filename.[position_start - 1] = '/')
        && (match line_number_opt with
            | None -> true
            | Some line_number -> def_line_number = line_number)
    in
    if found then used := true;
    found
  ) l

let print_delayed_errors () =
  match List.rev !delayed_errors with
  | [] -> ()
  | _ :: _ as delayed_errors ->
    Printf.eprintf "\n%s\n%!" (String.make 70 '=');
    List.iter (fun message ->
      Printf.eprintf "%s%!" message
    ) delayed_errors

let eprintf_or_delay fmt =
  Printf.ksprintf (fun s ->
    if !verbose then delayed_errors := s :: !delayed_errors
    else Printf.eprintf "%s%!" s;
    if !stop_on_error then begin
      print_delayed_errors ();
      exit 2
    end
  ) fmt

let test (descr : descr) def_filename def_line_number start_pos end_pos f =
  let descr () = displayed_descr descr def_filename def_line_number start_pos end_pos in
  match !action with
  | `Run_lib (lib, l) ->
    let should_run =
      Some lib = !dynamic_lib
      && begin match l with
      | [] -> true
      | _ :: _ -> position_match def_filename def_line_number l
      end in
    if should_run then begin
      let descr = descr () in
      incr tests_ran;
      begin match !log with
      | None -> ()
      | Some ch -> Printf.fprintf ch "%s\n%s" descr (string_of_module_descr ())
      end;
      if !verbose then begin
        Printf.printf "%s%!" descr
      end;
      let print_time_taken () =
        (* If !list_test_names, this is is a harmless zero. *)
        if !verbose then Printf.printf " (%.3f sec)\n%!" !time_sec;
      in
      try
        let failed = not !list_test_names && not (time f) in
        print_time_taken ();
        if failed then begin
          incr tests_failed;
          eprintf_or_delay "%s is false.\n%s\n%!" descr
            (string_of_module_descr ())
        end
      with exn ->
        print_time_taken ();
        let backtrace = backtrace_indented ~by:2 in
        incr tests_failed;
        let exn_str = Printexc.to_string exn in
        let sep = if String.contains exn_str '\n' then "\n" else " " in
        eprintf_or_delay "%s threw%s%s.\n%s%s\n%!" descr sep exn_str
          backtrace (string_of_module_descr ())
    end
  | `Ignore -> ()
  | `Collect r ->
    r := (fun () -> if not (time f) then failwith (descr ())) :: !r


let set_lib static_lib =
  match !dynamic_lib with
  | None -> dynamic_lib := Some static_lib
  | Some _ -> ()
    (* possible if the interface is used explicitly or if we happen to dynlink something
       that contain tests *)

let unset_lib static_lib =
  match !dynamic_lib with
  | None ->
    (* not giving an error, because when some annoying people put pa_ounit in their list
       of preprocessors, pa_ounit is set up twice and we have two calls to unset_lib at
       the end of the file, and the second one comes in this branch *)
    ()
  | Some lib ->
    if lib = static_lib then dynamic_lib := None

let test_unit descr def_filename def_line_number start_pos end_pos f =
  test descr def_filename def_line_number start_pos end_pos (fun () -> time f; true)

let collect f =
  let prev_action = !action in
  let tests = ref [] in
  action := `Collect tests;
  try
    time f;
    let tests = List.rev !tests in
    action := prev_action;
    tests
  with e ->
    action := prev_action;
    raise e

let test_module descr def_filename def_line_number start_pos end_pos f =
  let descr () = displayed_descr descr def_filename def_line_number start_pos end_pos in
  match !action with
  | `Run_lib (lib, _) ->
    (* run test_modules, in case they define the test we are looking for (if we are
       even looking for a test) *)
    if Some lib = !dynamic_lib then begin
      incr test_modules_ran;
      let descr = descr () in
      try with_descr descr f
      with exn ->
        let backtrace = backtrace_indented ~by:2 in
        incr test_modules_failed;
        let exn_str = Printexc.to_string exn in
        let sep = if String.contains exn_str '\n' then "\n" else " " in
        eprintf_or_delay ("TES" ^^ "T_MODULE at %s threw%s%s.\n%s%s\n%!")
          (String.uncapitalize descr) sep exn_str backtrace (string_of_module_descr ())
    end
  | `Ignore -> ()
  | `Collect r ->
    (* tEST_MODULE are going to be executed inline, unlike before *)
    r := List.rev_append (collect f) !r

let summarize () =
  match !action with
  | `Ignore ->
    if Sys.argv <> [||] && Filename.basename Sys.argv.(0) = "inline_tests_runner.exe" then
      Printf.eprintf "inline_tests_runner.exe is not supposed to be run by hand, you \n\
                      should run the inline_tests_runner script instead.\n%!"
    else
      Printf.eprintf "You are doing something unexpected with the tests. No tests have \n\
                      been run. You should use the inline_tests_runner script to run \n\
                      tests.\n%!";
    Test_result.Error
  | `Run_lib _
  | `Collect _ -> begin
      begin match !log with
      | None -> ()
      | Some ch -> close_out ch
      end;
      print_delayed_errors ();
      match !tests_failed, !test_modules_failed with
      | 0, 0 -> begin
          if !show_counts then begin
            Printf.eprintf "%d tests ran, %d test_modules ran\n%!" !tests_ran !test_modules_ran
          end;
          let errors =
            match !action with
            | `Run_lib (_, tests) ->
              let unused_tests =
                List.filter (fun (_, _, used) -> not !used) tests in
              begin match unused_tests with
              | [] -> None
              | _ :: _ -> Some unused_tests
              end
            | `Ignore
            | `Collect _ -> None in
          match errors with
          | Some tests ->
            Printf.eprintf "Pa_ounit error: the following -only-test flags matched nothing:";
            List.iter (fun (filename, line_number_opt, _) ->
              match line_number_opt with
              | None -> Printf.eprintf " %s" filename
              | Some line_number -> Printf.eprintf " %s:%d" filename line_number
            ) tests;
            Printf.eprintf ".\n%!";
            Test_result.Error
          | None ->
            if !tests_ran = 0 && !strict then begin
              Printf.eprintf "Pa_ounit error: no tests have been run.\n%!";
              Test_result.Error
            end else begin
              Test_result.Success
            end
        end
      | count, count_test_modules ->
        Printf.eprintf "FAILED %d / %d tests%s\n%!" count !tests_ran
          (if count_test_modules = 0 then "" else Printf.sprintf (", %d TES" ^^ "T_MODULES") count_test_modules);
        Test_result.Failure
    end

let use_color = !use_color
