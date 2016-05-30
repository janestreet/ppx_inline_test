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
end

let parse_argv argv l f msg =
  try
    Arg.parse_argv argv l f msg
  with
  | Arg.Bad msg -> Printf.eprintf "%s" msg; exit 1
  | Arg.Help msg -> Printf.printf "%s" msg; exit 0
;;

type tags = string list
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
type config = (module Inline_test_config.S)
type 'a test_function_args
   = config:config
  -> descr:descr
  -> tags:tags
  -> filename:filename
  -> line_number:line_number
  -> start_pos:start_pos
  -> end_pos:end_pos
  -> 'a
type which_tests =
  { only_test_location : (filename * line_number option * bool ref) list
  ; do_not_test_tags : tags
  ; partition : string option
  }
type test_mode =
  { libname : string
  ; what_to_do :
      [ `Run_tests of which_tests
      | `List_partitions
      ]
  }

module Action : sig
  type t = [
    | `Ignore
    | `Test_mode of test_mode
    | `Collect of (unit -> unit) list ref
  ]
  val get : unit -> t
  val set : t -> unit
end = struct
  type t = [
    | `Ignore
    | `Test_mode of test_mode
    | `Collect of (unit -> unit) list ref
  ]
  let action : t ref = ref `Ignore
  let force_drop =
    try ignore (Sys.getenv "FORCE_DROP_INLINE_TEST" : string); true
    with Not_found -> false
  let get () =
    (* This is useful when compiling to javascript.
       Js_of_ocaml can statically evaluate [Sys.getenv "FORCE_DROP_INLINE_TEST"]
       and inline the result ([`Ignore]) whenever [get ()] is called.
       Unit tests can then be treated as deadcode since the argument [f] of the [test]
       function below is never used. *)
    if force_drop
    then `Ignore
    else !action

  let set v = action := v
end

module Partition : sig
  val found_test : unit -> unit
  val set_current : string -> unit
  val is_current : string option -> bool
  val all : unit -> string list
end = struct
  let all = Hashtbl.create 23
  let current = ref ""  let set_current x = current := x
  let found_test () =
    if !current <> "" && not (Hashtbl.mem all !current) then
      Hashtbl.add all !current ()
  ;;
  let is_current = function
    | None -> true
    | Some p -> p = !current
  ;;
  let all () =
    List.sort String.compare
      (Hashtbl.fold (fun k () acc -> k :: acc) all [])
  ;;
end

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
let in_place  = ref false
let diff_command = ref None

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
    let list_partitions = ref false in
    let partition = ref None in
    let disabled_tags = ref [] in
    parse_argv (Array.of_list (name :: rest)) (Arg.align [
      "-list-test-names", Arg.Unit (fun () -> list_test_names := true; verbose := true),
        " Do not run tests but show what would have been run";
      "-list-partitions", Arg.Unit (fun () -> list_partitions := true),
        " Lists all the partitions that contain at least one test or test_module";
      "-partition", Arg.String (fun i -> partition := Some i),
        " Only run the tests in the given partition";
      "-verbose", Arg.Set verbose, " Show the tests as they run";
      "-stop-on-error", Arg.Set stop_on_error, " Run tests only up to the first error";
      "-strict", Arg.Set strict, " End with an error if no tests were run";
      "-show-counts", Arg.Set show_counts, " Show the number of tests ran";
      "-log", Arg.Unit (fun () ->
        (try Sys.remove "inline_tests.log" with _ -> ());
        log := Some (open_out "inline_tests.log")
      ), " Log the tests run in inline_tests.log";
      "-drop-tag", Arg.String (fun s ->
        disabled_tags := s :: !disabled_tags
      ), "tag Ignore tests tagged with [tag].";
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
      "-no-color", Arg.Clear use_color, " Summarize tests without using color";
      "-in-place", Arg.Set in_place, " Update expect tests in place";
      "-diff-cmd", Arg.String (fun s -> diff_command := Some s),
      " Diff command for tests that require diffing";
    ]) (fun anon ->
      Printf.eprintf "%s: unexpected anonymous argument %s\n%!" name anon;
      exit 1
    ) (Printf.sprintf "%s %s %s [args]" name "inline-test-runner" lib);
    Action.set (
      `Test_mode
        { libname = lib
        ; what_to_do =
            if !list_partitions
            then `List_partitions
            else
              `Run_tests { only_test_location = !tests;
                           do_not_test_tags = !disabled_tags;
                           partition = !partition }
        })
    end
  | _ ->
    ()

let testing =
  match Action.get () with
  | `Test_mode _ -> true
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

let not_disabled tags do_not_test_tags =
  not (List.exists (fun gr -> List.mem gr do_not_test_tags) tags)

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

let add_hooks ((module C) : config) f =
  fun () -> C.pre_test_hook (); f ()

let test ~config ~descr ~tags ~filename:def_filename ~line_number:def_line_number
      ~start_pos ~end_pos f =
  let f = add_hooks config f in
  let descr () = displayed_descr descr def_filename def_line_number start_pos end_pos in
  match Action.get () with
  | `Test_mode { libname; what_to_do } ->
    if Some libname = !dynamic_lib then begin
      match what_to_do with
      | `List_partitions -> Partition.found_test ()
      | `Run_tests { only_test_location; do_not_test_tags; partition } ->
        let should_run =
          begin match only_test_location with
          | [] -> true
          | _ :: _ -> position_match def_filename def_line_number only_test_location
          end
          && not_disabled tags do_not_test_tags
          && Partition.is_current partition
       in
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
   end
  | `Ignore -> ()
  | `Collect r ->
    r := (fun () -> if not (time f) then failwith (descr ())) :: !r

let set_lib_and_partition static_lib partition =
  match !dynamic_lib with
  | Some _ ->
    (* possible if the interface is used explicitly or if we happen to dynlink something
       that contain tests *)
    ()
  | None ->
    dynamic_lib := Some static_lib;
    match Action.get () with
    | `Collect _ | `Ignore -> ()
    | `Test_mode { libname; what_to_do } ->
      if libname = static_lib then begin
        let requires_partition =
          match what_to_do with
          | `List_partitions | `Run_tests { partition = Some _; _ } -> true
          | `Run_tests { partition = None; _ } -> false
        in
        if partition = "" && requires_partition
        then failwith "ppx_inline_test: cannot use -list-partition or -partition \
                       without specifying a partition at preprocessing time"
        else Partition.set_current partition
      end

let unset_lib static_lib =
  match !dynamic_lib with
  | None ->
    (* not giving an error, because when some annoying people put pa_ounit in their list
       of preprocessors, pa_ounit is set up twice and we have two calls to unset_lib at
       the end of the file, and the second one comes in this branch *)
    ()
  | Some lib ->
    if lib = static_lib then dynamic_lib := None

let test_unit ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos f =
  test ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos
    (fun () -> time f; true)

let collect f =
  let prev_action = Action.get () in
  let tests = ref [] in
  Action.set (`Collect tests);
  try
    time f;
    let tests = List.rev !tests in
    Action.set prev_action;
    tests
  with e ->
    Action.set prev_action;
    raise e

let test_module ~config ~descr ~tags ~filename:def_filename ~line_number:def_line_number
      ~start_pos ~end_pos f =
  let f = add_hooks config f in
  let descr () = displayed_descr descr def_filename def_line_number start_pos end_pos in
  match Action.get () with
  | `Test_mode { libname; what_to_do } ->
    if Some libname = !dynamic_lib then begin
      match what_to_do with
      | `List_partitions -> Partition.found_test ()
      | `Run_tests { do_not_test_tags; partition; _ } ->
        (* run test_modules, in case they define the test we are looking for (if we are
           even looking for a test) *)
        if not_disabled tags do_not_test_tags
        && Partition.is_current partition
        then begin
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
    end
  | `Ignore -> ()
  | `Collect r ->
    (* tEST_MODULE are going to be executed inline, unlike before *)
    r := List.rev_append (collect f) !r

let summarize () =
  match Action.get () with
  | `Ignore ->
    if Sys.argv <> [||] && Filename.basename Sys.argv.(0) = "inline_tests_runner.exe" then
      Printf.eprintf "inline_tests_runner.exe is not supposed to be run by hand, you \n\
                      should run the inline_tests_runner script instead.\n%!"
    else
      Printf.eprintf "You are doing something unexpected with the tests. No tests have \n\
                      been run. You should use the inline_tests_runner script to run \n\
                      tests.\n%!";
    Test_result.Error
  | `Test_mode { libname = _ ; what_to_do = `List_partitions } ->
    List.iter (Printf.printf "%s\n") (Partition.all ());
    Test_result.Success
  | (`Test_mode _ | `Collect _ as action) -> begin
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
            match action with
            | `Collect _ -> None
            | `Test_mode { what_to_do = `List_partitions; _ } -> assert false
            | `Test_mode { what_to_do = `Run_tests { only_test_location; _}; _ } ->
              let unused_tests =
                List.filter (fun (_, _, used) -> not !used) only_test_location in
              match unused_tests with
              | [] -> None
              | _ :: _ -> Some unused_tests
          in
          match errors with
          | Some tests ->
            Printf.eprintf "ppx_inline_test error: the following -only-test flags matched nothing:";
            List.iter (fun (filename, line_number_opt, _) ->
              match line_number_opt with
              | None -> Printf.eprintf " %s" filename
              | Some line_number -> Printf.eprintf " %s:%d" filename line_number
            ) tests;
            Printf.eprintf ".\n%!";
            Test_result.Error
          | None ->
            if !tests_ran = 0 && !strict then begin
              Printf.eprintf "ppx_inline_test error: no tests have been run.\n%!";
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
let in_place  = !in_place
let diff_command = !diff_command

let evaluators = ref [summarize]
let add_evaluator ~f = evaluators := f :: !evaluators
let exit () =
  List.map (fun f -> f ()) (List.rev !evaluators)
  |> Test_result.combine_all
  |> Test_result.to_exit_code
  |> exit
