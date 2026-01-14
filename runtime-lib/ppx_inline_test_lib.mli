@@ portable

(** [am_running] is [true] if the code is running inline tests (e.g. [let%expect_test],
    [let%test], [let%test_unit]) or is in an executable invoked from inline tests. *)
val am_running : bool

(** [am_test_runner] is [true] if the code is running inline tests (e.g.
    [let%expect_test], [let%test], [let%test_unit]); unlike [am_running], this is [false]
    for an executable invoked from inline tests. *)
val am_test_runner : unit -> bool

val am_running_env_var : string

(** [`Am_test_runner] means the [./inline_tests_runner] process, whereas
    [`Am_child_of_test_runner] means a process descended from the test runner. *)
val testing
  :  unit
  -> [ `Not_testing | `Testing of [ `Am_test_runner | `Am_child_of_test_runner ] ]

(** The tests to run are configured by command line arguments, normally pulled from
    [Sys.argv]. Calling [init] will re-configure the test runner using the passed-in
    argument list. This is useful to run tests in a dynamically loaded library; this
    should be called with the appropriate configuration before loading the library.

    [init] will normally return None. It will return an error if there's a formatting
    error in the arguments, and will return Some string if help was requested. It will
    also return an error if the test runner has already initialized, either by reading
    command-line arguments or by a previous call to [init]. *)
val init : string list -> (string option, string) result @@ nonportable

(**/**)

(** Everything below is for ppx or internal use *)

module Test_result : sig
  type t =
    | Success
    | Failure
    | Error

  val combine : t -> t -> t
  val combine_all : t list -> t
  val to_string : t -> string
end

type config = (module Inline_test_config.S)

type 'a test_function_args =
  config:config
  -> descr:string Lazy.t
  -> tags:string list
  -> filename:string
  -> line_number:int
  -> start_pos:int
  -> end_pos:int
  -> 'a

val set_lib_and_partition : string -> string -> unit @@ nonportable
val unset_lib : string -> unit @@ nonportable
val test : ((unit -> bool) -> unit) test_function_args @@ nonportable
val test_unit : ((unit -> unit) -> unit) test_function_args @@ nonportable
val test_module : ((unit -> unit) -> unit) test_function_args @@ nonportable
val verbose : unit -> bool
val use_color : unit -> bool
val in_place : unit -> bool
val diff_command : unit -> string option
val diff_path_prefix : unit -> string option
val source_tree_root : unit -> string option

(** This value is [true] if [FORCE_DROP_INLINE_TEST] was set at startup time. This is
    useful when compiling to javascript --- Js_of_ocaml can statically evaluate
    [Sys.getenv "FORCE_DROP_INLINE_TEST"]. Unit tests visibly only reachable if
    [force_drop] is [true] can then be treated as deadcode.

    It is guaranteed that, if [force_drop = true], no test registered via one of the
    [test] functions above will run.

    [force_drop] is only exposed so that other ppxs can generate code that checks it more
    in sight of JSOO's DCE. [force_drop] is otherwise not a recommended method of changing
    the behavior of a program based on whether test are running. *)
val force_drop : bool

(** Record an evaluator for an external set of tests *)
val add_evaluator : f:(unit -> Test_result.t) -> unit @@ nonportable

(** Exit with a status based on the combined result of all recorded evaluators *)
val exit : unit -> _ @@ nonportable

(** Like [exit], but just return the exit status that would have been used *)
val evaluate_exit_status : unit -> int @@ nonportable
