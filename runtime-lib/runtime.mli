
module Test_result : sig
  type t = Success | Failure | Error

  val combine : t -> t -> t
  val combine_all : t list -> t

  val to_string : t -> string
end

type config = (module Inline_test_config.S)
type 'a test_function_args
   = config:config
  -> descr:string
  -> tags:string list
  -> filename:string
  -> line_number:int
  -> start_pos:int
  -> end_pos:int
  -> 'a
val set_lib_and_partition : string -> string -> unit
val unset_lib : string -> unit
val test : ((unit -> bool) -> unit) test_function_args
val test_unit : ((unit -> unit) -> unit) test_function_args
val test_module : ((unit -> unit) -> unit) test_function_args
val summarize : unit -> Test_result.t
  [@@deprecated "[since 2016-04] use add_evaluator instead"]

(* These functions are meant to be called by hand, they should be in an other module. *)
val collect : (unit -> unit) -> (unit -> unit) list
val testing : bool
val use_color : bool
val in_place : bool
val diff_command : string option

(** Record an evaluator for an external set of tests *)
val add_evaluator : f:(unit -> Test_result.t) -> unit

(** Exit with a status based on the combined result of all recorded evaluators *)
val exit : unit -> _
