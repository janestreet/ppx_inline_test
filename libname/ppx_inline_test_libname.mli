(** This library defines the command line argument -inline-test-lib (and ppxlib cookie
    library-name), shared by both ppx_bench and ppx_inline_test. *)
val get : unit -> (string * string option) option
