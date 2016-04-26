
module Test_result : sig
  type t = Success | Failure | Error

  val combine : t -> t -> t
  val combine_all : t list -> t

  val to_string : t -> string
end

type descr = string
type filename = string
type line_number = int
type start_pos = int
type end_pos = int
type config = (module Inline_test_config.S)
val set_lib : string -> unit
val unset_lib : string -> unit
val test : config -> descr -> filename -> line_number -> start_pos -> end_pos -> (unit -> bool) -> unit
val test_unit : config -> descr -> filename -> line_number -> start_pos -> end_pos -> (unit -> unit) -> unit
val test_module : config -> descr -> filename -> line_number -> start_pos -> end_pos -> (unit -> unit) -> unit
val summarize : unit -> Test_result.t
  [@@deprecated "[since 2016-04] use eval_all_and_exit instead"]

(* These functions are meant to be called by hand, they should be in an other module. *)
val collect : (unit -> unit) -> (unit -> unit) list
val testing : bool
val use_color : bool
val diff_command : string option

(** Record an evaluautor for an external set of tests *)
val add_evaluator : f:(unit -> Test_result.t) -> unit

(** Exit with a status based on the combined result of all recorded evaluators *)
val exit : unit -> _
