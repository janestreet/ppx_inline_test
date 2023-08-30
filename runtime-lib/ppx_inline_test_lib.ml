module Test_result = struct
  type t =
    | Success
    | Failure
    | Error

  let to_exit_code = function
    | Success -> 0
    | Failure -> 2
    | Error -> 1
  ;;

  let to_string = function
    | Success -> "success"
    | Failure -> "failure"
    | Error -> "error"
  ;;

  let combine t1 t2 =
    match t1, t2 with
    | Success, Success -> Success
    | Error, _ | _, Error -> Error
    | Failure, _ | _, Failure -> Failure
  ;;

  let combine_all ts = List.fold_left combine Success ts
end

let parse_argv argv l f msg =
  try Arg.parse_argv argv l f msg with
  | Arg.Bad msg ->
    Printf.eprintf "%s" msg;
    exit 1
  | Arg.Help msg ->
    Printf.printf "%s" msg;
    exit 0
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
type config = (module Inline_test_config.S)

type 'a test_function_args =
  config:config
  -> descr:descr Lazy.t
  -> tags:string list
  -> filename:filename
  -> line_number:line_number
  -> start_pos:start_pos
  -> end_pos:end_pos
  -> 'a

module Tag_predicate = struct
  type t =
    { required_tags : string list
    ; dropped_tags : string list
    }

  let initial = { required_tags = []; dropped_tags = [ "disabled" ] }

  let drop t tag =
    { dropped_tags = tag :: t.dropped_tags
    ; required_tags = List.filter (( <> ) tag) t.required_tags
    }
  ;;

  let require t tag =
    { dropped_tags = List.filter (( <> ) tag) t.dropped_tags
    ; required_tags = tag :: t.required_tags
    }
  ;;

  let entire_module_disabled t ~partial_tags:tags =
    List.exists (fun dropped -> List.mem dropped tags) t.dropped_tags
  ;;

  let disabled t ~complete_tags:tags =
    List.exists (fun req -> not (List.mem req tags)) t.required_tags
    || List.exists (fun dropped -> List.mem dropped tags) t.dropped_tags
  ;;
end

type which_tests =
  { libname : string
  ; only_test_location : (filename * line_number option * bool ref) list
  ; name_filter : string list
  ; which_tags : Tag_predicate.t
  }

type test_mode =
  { which_tests : which_tests
  ; what_to_do : [ `Run_partition of string option | `List_partitions ]
  }

module Action : sig
  type t =
    [ `Ignore
    | `Test_mode of test_mode
    ]

  val get : unit -> t
  val set : t -> unit
end = struct
  type t =
    [ `Ignore
    | `Test_mode of test_mode
    ]

  let action : t ref = ref `Ignore

  let force_drop =
    try
      ignore (Sys.getenv "FORCE_DROP_INLINE_TEST" : string);
      true
    with
    | Not_found -> false
  ;;

  let get () =
    (* This is useful when compiling to javascript.
       Js_of_ocaml can statically evaluate [Sys.getenv "FORCE_DROP_INLINE_TEST"]
       and inline the result ([`Ignore]) whenever [get ()] is called.
       Unit tests can then be treated as deadcode since the argument [f] of the [test]
       function below is never used. *)
    if force_drop then `Ignore else !action
  ;;

  let set v = action := v
end

module Partition : sig
  val found_test : unit -> unit
  val set_current : string -> unit
  val is_current : string option -> bool
  val all : unit -> string list
end = struct
  let all = Hashtbl.create 23
  let current = ref ""
  let set_current x = current := x

  let found_test () =
    if !current <> "" && not (Hashtbl.mem all !current) then Hashtbl.add all !current ()
  ;;

  let is_current = function
    | None -> true
    | Some p -> p = !current
  ;;

  let all () = List.sort String.compare (Hashtbl.fold (fun k () acc -> k :: acc) all [])
end

module Module_context = struct
  module T = struct
    type one_module =
      { descr : string
      ; tags : string list
      }

    type t = one_module list

    let descr t = List.map (fun m -> m.descr) t
    let tags t = List.concat (List.map (fun m -> m.tags) t)
  end

  let current : T.t ref = ref []

  let with_ ~descr ~tags f =
    let prev = !current in
    current := { T.descr; tags } :: prev;
    try
      let x = f () in
      current := prev;
      x
    with
    | e ->
      current := prev;
      raise e
  ;;

  let current_descr () = T.descr !current
  let current_tags () = T.tags !current
end

let verbose = ref false
let strict = ref false
let show_counts = ref false
let list_test_names = ref false
let delayed_errors = ref []
let stop_on_error = ref false
let log = ref None
let time_sec = ref 0.
let use_color = ref true
let in_place = ref false
let diff_command = ref None
let source_tree_root = ref None
let diff_path_prefix = ref None

let displayed_descr descr filename line start_pos end_pos =
  let (lazy descr) = descr in
  Printf.sprintf
    "File %S, line %d, characters %d-%d%s"
    filename
    line
    start_pos
    end_pos
    (if descr = "" then "" else ": " ^ descr)
;;

let parse_descr str =
  try
    Some
      (Scanf.sscanf
         str
         " File %S , line %d , characters %d - %d %!"
         (fun file line _start_pos _end_pos -> file, Some line))
  with
  | _ ->
    (try
       Some (Scanf.sscanf str " File %S , line %d %!" (fun file line -> file, Some line))
     with
     | _ ->
       (try Some (Scanf.sscanf str " File %S %!" (fun file -> file, None)) with
        | _ -> None))
;;

let () =
  if Base.Exported_for_specific_uses.am_testing
  then (
    match Array.to_list Sys.argv with
    | name :: "inline-test-runner" :: lib :: rest ->
      (* when we see this argument, we switch to test mode *)
      let tests = ref [] in
      let list_partitions = ref false in
      let partition = ref None in
      let tag_predicate = ref Tag_predicate.initial in
      let name_filter = ref [] in
      parse_argv
        (Array.of_list (name :: rest))
        (Arg.align
           [ ( "-list-test-names"
             , Arg.Unit
                 (fun () ->
                   list_test_names := true;
                   verbose := true)
             , " Do not run tests but show what would have been run" )
           ; ( "-list-partitions"
             , Arg.Unit (fun () -> list_partitions := true)
             , " Lists all the partitions that contain at least one test or test_module" )
           ; ( "-partition"
             , Arg.String (fun i -> partition := Some i)
             , " Only run the tests in the given partition" )
           ; "-verbose", Arg.Set verbose, " Show the tests as they run"
           ; ( "-stop-on-error"
             , Arg.Set stop_on_error
             , " Run tests only up to the first error (doesn't work for expect tests)" )
           ; "-strict", Arg.Set strict, " End with an error if no tests were run"
           ; "-show-counts", Arg.Set show_counts, " Show the number of tests ran"
           ; ( "-log"
             , Arg.Unit
                 (fun () ->
                   (try Sys.remove "inline_tests.log" with
                    | _ -> ());
                   log := Some (open_out "inline_tests.log"))
             , " Log the tests run in inline_tests.log" )
           ; ( "-drop-tag"
             , Arg.String (fun s -> tag_predicate := Tag_predicate.drop !tag_predicate s)
             , "tag Only run tests not tagged with [tag] (overrides previous \
                -require-tag)" )
           ; ( "-require-tag"
             , Arg.String
                 (fun s -> tag_predicate := Tag_predicate.require !tag_predicate s)
             , "tag Only run tests tagged with [tag] (overrides previous -drop-tag)" )
           ; ( "-matching"
             , Arg.String (fun s -> name_filter := s :: !name_filter)
             , "substring Only run tests whose names contain the given substring" )
           ; ( "-only-test"
             , Arg.String
                 (fun s ->
                   let filename, index =
                     match parse_descr s with
                     | Some (file, index) -> file, index
                     | None ->
                       if String.contains s ':'
                       then (
                         let i = String.index s ':' in
                         let filename = String.sub s 0 i in
                         let index_string =
                           String.sub s (i + 1) (String.length s - i - 1)
                         in
                         let index =
                           try int_of_string index_string with
                           | Failure _ ->
                             Printf.eprintf
                               "Argument %s doesn't fit the format filename[:line_number]\n\
                                %!"
                               s;
                             exit 1
                         in
                         filename, Some index)
                       else s, None
                   in
                   tests := (filename, index, ref false) :: !tests)
             , "location Run only the tests specified by all the -only-test options.\n\
               \                      Locations can be one of these forms:\n\
               \                      - file.ml\n\
               \                      - file.ml:line_number\n\
               \                      - File \"file.ml\"\n\
               \                      - File \"file.ml\", line 23\n\
               \                      - File \"file.ml\", line 23, characters 2-3" )
           ; "-no-color", Arg.Clear use_color, " Summarize tests without using color"
           ; "-in-place", Arg.Set in_place, " Update expect tests in place"
           ; ( "-diff-cmd"
             , Arg.String (fun s -> diff_command := Some s)
             , " Diff command for tests that require diffing (use - to disable diffing)" )
           ; ( "-source-tree-root"
             , Arg.String (fun s -> source_tree_root := Some s)
             , " Path to the root of the source tree" )
           ; ( "-diff-path-prefix"
             , Arg.String (fun s -> diff_path_prefix := Some s)
             , " Prefix to prepend to filepaths in test output" )
           ])
        (fun anon ->
          Printf.eprintf "%s: unexpected anonymous argument %s\n%!" name anon;
          exit 1)
        (Printf.sprintf "%s %s %s [args]" name "inline-test-runner" lib);
      Action.set
        (`Test_mode
          { which_tests =
              { libname = lib
              ; only_test_location = !tests
              ; which_tags = !tag_predicate
              ; name_filter = !name_filter
              }
          ; what_to_do =
              (if !list_partitions then `List_partitions else `Run_partition !partition)
          })
    | _ -> ())
;;

let am_test_runner =
  match Action.get () with
  | `Test_mode _ -> true
  | `Ignore -> false
;;

let am_running_env_var =
  (* for approximate compatibility, given that the variable is not exactly equivalent
     to what PPX_INLINE_TEST_LIB_AM_RUNNING_INLINE_TEST used to be *)
  "TESTING_FRAMEWORK"
;;

(* This value is deprecated in principle, in favor of Core.am_running_test, so
   we're going to live with the ugly pattern match. *)
let am_running =
  match Sys.getenv "PPX_INLINE_TEST_LIB_AM_RUNNING_INLINE_TEST" with
  | (_ : string) ->
    true (* for compatibility with people setting this variable directly *)
  | exception Not_found ->
    (match Sys.getenv am_running_env_var with
     | "inline-test" -> true
     | exception Not_found -> false
     | _ -> false)
;;

let testing =
  if am_test_runner
  then `Testing `Am_test_runner
  else if am_running
  then `Testing `Am_child_of_test_runner
  else `Not_testing
;;

let wall_time_clock_ns () = Time_now.nanoseconds_since_unix_epoch ()

let where_to_cut_backtrace =
  lazy
    (Base.String.Search_pattern.create
       (__MODULE__ ^ "." ^ "time_without_resetting_random_seeds"))
;;

let time_without_resetting_random_seeds f =
  let before_ns = wall_time_clock_ns () in
  let res =
    (* To avoid noise in backtraces, we do two things.

       We use [where_to_cut_backtrace] above to remove the stack frames for the current
       function and any function it gets inlined into, as it's not of any interest to the
       user, since it's not talking about their test but instead talking about the
       ppx_inline_test machinery.

       We also avoid inserting any code between the [f] that comes from the user's file
       and grabbing the backtrace from its exceptions (no wrapping of [f] with high order
       functions like Exn.protect, or (fun () -> f (); true)). *)
    try Ok (f ()) with
    | exn -> Error (exn, Printexc.get_backtrace ())
  in
  time_sec := Base.Int63.(wall_time_clock_ns () - before_ns |> to_float) /. 1e9;
  res
;;

let saved_caml_random_state = lazy (Stdlib.Random.State.make [| 100; 200; 300 |])
let saved_base_random_state = lazy (Base.Random.State.make [| 111; 222; 333 |])

let time_and_reset_random_seeds f =
  let caml_random_state = Stdlib.Random.get_state () in
  let base_random_state = Base.Random.State.copy Base.Random.State.default in
  Stdlib.Random.set_state (Lazy.force saved_caml_random_state);
  Base.Random.set_state (Lazy.force saved_base_random_state);
  let result = time_without_resetting_random_seeds f in
  Stdlib.Random.set_state caml_random_state;
  Base.Random.set_state base_random_state;
  result
;;

let string_of_module_descr () =
  String.concat
    ""
    (List.map
       (fun s -> "  in TES" ^ "T_MODULE at " ^ String.uncapitalize_ascii s ^ "\n")
       (Module_context.current_descr ()))
;;

let position_match def_filename def_line_number l =
  List.exists
    (fun (filename, line_number_opt, used) ->
      let position_start = String.length def_filename - String.length filename in
      let found =
        position_start >= 0
        &&
        let end_of_def_filename =
          String.sub def_filename position_start (String.length filename)
        in
        end_of_def_filename = filename
        && (position_start = 0 || def_filename.[position_start - 1] = '/')
        &&
        match line_number_opt with
        | None -> true
        | Some line_number -> def_line_number = line_number
      in
      if found then used := true;
      found)
    l
;;

let name_filter_match ~name_filter descr =
  match name_filter with
  | [] -> true
  | _ :: _ ->
    List.exists (fun substring -> Base.String.is_substring ~substring descr) name_filter
;;

let print_delayed_errors () =
  match List.rev !delayed_errors with
  | [] -> ()
  | _ :: _ as delayed_errors ->
    Printf.eprintf "\n%s\n%!" (String.make 70 '=');
    List.iter (fun message -> Printf.eprintf "%s%!" message) delayed_errors
;;

let eprintf_or_delay fmt =
  Printf.ksprintf
    (fun s ->
      if !verbose then delayed_errors := s :: !delayed_errors else Printf.eprintf "%s%!" s;
      if !stop_on_error
      then (
        print_delayed_errors ();
        exit 2))
    fmt
;;

let add_hooks ((module C) : config) f () =
  C.pre_test_hook ();
  f ()
;;

let hum_backtrace backtrace =
  let open Base in
  backtrace
  |> String.split_lines
  |> List.take_while ~f:(fun str ->
       not (String.Search_pattern.matches (force where_to_cut_backtrace) str))
  |> List.map ~f:(fun str -> "  " ^ str ^ "\n")
  |> String.concat
;;

let[@inline never] test_inner
  ~config
  ~descr
  ~tags
  ~filename:def_filename
  ~line_number:def_line_number
  ~start_pos
  ~end_pos
  f
  bool_of_f
  =
  match Action.get () with
  | `Ignore -> ()
  | `Test_mode
      { which_tests = { libname; only_test_location; which_tags; name_filter }
      ; what_to_do
      } ->
    let f = add_hooks config f in
    let descr =
      lazy (displayed_descr descr def_filename def_line_number start_pos end_pos)
    in
    let complete_tags = tags @ Module_context.current_tags () in
    let should_run =
      Some libname = !dynamic_lib
      && (match only_test_location with
          | [] -> true
          | _ :: _ -> position_match def_filename def_line_number only_test_location)
      && (not (Tag_predicate.disabled which_tags ~complete_tags))
      && name_filter_match ~name_filter (Lazy.force descr)
    in
    if should_run
    then (
      match what_to_do with
      | `List_partitions -> Partition.found_test ()
      | `Run_partition partition ->
        if Partition.is_current partition
        then (
          let descr = Lazy.force descr in
          incr tests_ran;
          (match !log with
           | None -> ()
           | Some ch -> Printf.fprintf ch "%s\n%s" descr (string_of_module_descr ()));
          if !verbose then Printf.printf "%s%!" descr;
          let result =
            if !list_test_names
            then Ok true
            else
              (* See [time_without_resetting_random_seeds] for why we use [bool_of_f]
                 rather have the caller wrap [f] to adjust its return value. *)
              Result.map bool_of_f (time_and_reset_random_seeds f)
          in
          (* If !list_test_names, this is is a harmless zero. *)
          if !verbose then Printf.printf " (%.3f sec)\n%!" !time_sec;
          match result with
          | Ok true -> ()
          | Ok false ->
            incr tests_failed;
            eprintf_or_delay "%s is false.\n%s\n%!" descr (string_of_module_descr ())
          | Error (exn, backtrace) ->
            incr tests_failed;
            let backtrace = hum_backtrace backtrace in
            let exn_str = Sexplib0.Sexp_conv.printexc_prefer_sexp exn in
            let sep = if String.contains exn_str '\n' then "\n" else " " in
            eprintf_or_delay
              "%s threw%s%s.\n%s%s\n%!"
              descr
              sep
              exn_str
              backtrace
              (string_of_module_descr ())))
;;

let set_lib_and_partition static_lib partition =
  match !dynamic_lib with
  | Some _ ->
    (* possible if the interface is used explicitly or if we happen to dynlink something
       that contain tests *)
    ()
  | None ->
    dynamic_lib := Some static_lib;
    (match Action.get () with
     | `Ignore -> ()
     | `Test_mode { which_tests; what_to_do } ->
       if which_tests.libname = static_lib
       then (
         let requires_partition =
           match what_to_do with
           | `List_partitions | `Run_partition (Some _) -> true
           | `Run_partition None -> false
         in
         if partition = "" && requires_partition
         then
           failwith
             "ppx_inline_test: cannot use -list-partition or -partition without \
              specifying a partition at preprocessing time"
         else Partition.set_current partition))
;;

let unset_lib static_lib =
  match !dynamic_lib with
  | None ->
    (* not giving an error, because when some annoying people put pa_ounit in their list
       of preprocessors, pa_ounit is set up twice and we have two calls to unset_lib at
       the end of the file, and the second one comes in this branch *)
    ()
  | Some lib -> if lib = static_lib then dynamic_lib := None
;;

let test ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos f =
  test_inner ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos f (fun b ->
    b)
;;

let test_unit ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos f =
  test_inner ~config ~descr ~tags ~filename ~line_number ~start_pos ~end_pos f (fun () ->
    true)
;;

let[@inline never] test_module
  ~config
  ~descr
  ~tags
  ~filename:def_filename
  ~line_number:def_line_number
  ~start_pos
  ~end_pos
  f
  =
  match Action.get () with
  | `Ignore -> ()
  | `Test_mode
      { which_tests = { libname; only_test_location = _; name_filter = _; which_tags }
      ; what_to_do
      } ->
    let f = add_hooks config f in
    let descr () = displayed_descr descr def_filename def_line_number start_pos end_pos in
    let partial_tags = tags @ Module_context.current_tags () in
    let should_run =
      Some libname = !dynamic_lib
      (* If, no matter what tags a test defines, we certainly will drop all tests within
         this module, then don't run the module at all. This means people can write
         things like the following without breaking the 32-bit build:
         let%test_module [@tags "64-bits-only"] = (module struct
         let i = Int64.to_int_exn ....
         end)
         We don't shortcut based on position, as we can't tell what positions the
         inner tests will have. *)
      && not (Tag_predicate.entire_module_disabled which_tags ~partial_tags)
    in
    if should_run
    then (
      match what_to_do with
      | `List_partitions -> Partition.found_test ()
      | `Run_partition partition ->
        if Partition.is_current partition
        then (
          incr test_modules_ran;
          let descr = descr () in
          match
            Module_context.with_ ~descr ~tags (fun () ->
              (* We do not reset random states upon entering [let%test_module].

                 Con: Code in test modules can accidentally depend on top-level random
                 state effects.

                 Pros: (1) We don't reset to the same seed on entering a [let%test_module]
                 and then a [let%test] inside that module, which could lead to
                 accidentally randomly generating the same values in some test. (2) Moving
                 code into and out of [let%test_module] does not change its random seed.
              *)
              time_without_resetting_random_seeds f)
          with
          | Ok () -> ()
          | Error (exn, backtrace) ->
            incr test_modules_failed;
            let backtrace = hum_backtrace backtrace in
            let exn_str = Sexplib0.Sexp_conv.printexc_prefer_sexp exn in
            let sep = if String.contains exn_str '\n' then "\n" else " " in
            eprintf_or_delay
              ("TES" ^^ "T_MODULE at %s threw%s%s.\n%s%s\n%!")
              (String.uncapitalize_ascii descr)
              sep
              exn_str
              backtrace
              (string_of_module_descr ())))
;;

let summarize () =
  match Action.get () with
  | `Ignore ->
    if Sys.argv <> [||] && Filename.basename Sys.argv.(0) = "inline_tests_runner.exe"
    then
      Printf.eprintf
        "inline_tests_runner.exe is not supposed to be run by hand, you \n\
         should run the inline_tests_runner script instead.\n\
         %!"
    else
      Printf.eprintf
        "You are doing something unexpected with the tests. No tests have \n\
         been run. You should use the inline_tests_runner script to run \n\
         tests.\n\
         %!";
    Test_result.Error
  | `Test_mode { which_tests = _; what_to_do = `List_partitions } ->
    List.iter (Printf.printf "%s\n") (Partition.all ());
    Test_result.Success
  | `Test_mode { what_to_do = `Run_partition _; which_tests } ->
    (match !log with
     | None -> ()
     | Some ch -> close_out ch);
    print_delayed_errors ();
    (match !tests_failed, !test_modules_failed with
     | 0, 0 ->
       if !show_counts
       then
         Printf.eprintf
           "%d tests ran, %d test_modules ran\n%!"
           !tests_ran
           !test_modules_ran;
       let errors =
         let unused_tests =
           List.filter (fun (_, _, used) -> not !used) which_tests.only_test_location
         in
         match unused_tests with
         | [] -> None
         | _ :: _ -> Some unused_tests
       in
       (match errors with
        | Some tests ->
          Printf.eprintf
            "ppx_inline_test error: the following -only-test flags matched nothing:";
          List.iter
            (fun (filename, line_number_opt, _) ->
              match line_number_opt with
              | None -> Printf.eprintf " %s" filename
              | Some line_number -> Printf.eprintf " %s:%d" filename line_number)
            tests;
          Printf.eprintf ".\n%!";
          Test_result.Error
        | None ->
          if !tests_ran = 0 && !strict
          then (
            Printf.eprintf "ppx_inline_test error: no tests have been run.\n%!";
            Test_result.Error)
          else Test_result.Success)
     | count, count_test_modules ->
       Printf.eprintf
         "FAILED %d / %d tests%s\n%!"
         count
         !tests_ran
         (if count_test_modules = 0
          then ""
          else Printf.sprintf (", %d TES" ^^ "T_MODULES") count_test_modules);
       Test_result.Failure)
;;

let use_color = !use_color
let in_place = !in_place
let diff_command = !diff_command
let diff_path_prefix = !diff_path_prefix
let source_tree_root = !source_tree_root
let evaluators = ref [ summarize ]
let add_evaluator ~f = evaluators := f :: !evaluators

let exit () =
  List.map (fun f -> f ()) (List.rev !evaluators)
  |> Test_result.combine_all
  |> Test_result.to_exit_code
  |> exit
;;
