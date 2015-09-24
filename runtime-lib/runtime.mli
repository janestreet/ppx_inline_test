type descr = string
type filename = string
type line_number = int
type start_pos = int
type end_pos = int
val set_lib : string -> unit
val unset_lib : string -> unit
val test : descr -> filename -> line_number -> start_pos -> end_pos -> (unit -> bool) -> unit
val test_unit : descr -> filename -> line_number -> start_pos -> end_pos -> (unit -> unit) -> unit
val test_module : descr -> filename -> line_number -> start_pos -> end_pos -> (unit -> unit) -> unit
val summarize : unit -> unit

(* These functions are meant to be called by hand, they should be in an other module. *)
val collect : (unit -> unit) -> (unit -> unit) list
val testing : bool
