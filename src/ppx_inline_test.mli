open Ppxlib

type maybe_drop =
  | Keep
  | Drop_with_deadcode
  | Drop

(** How to expand tests if no "-inline-test-drop*" command line flag is passed. *)
val set_default_maybe_drop : maybe_drop -> unit

(** To be called on test extension points that use the ppx_inline_test runtime. Checks
    that tests are allowed with the given ppx command line, and that the tags are defined. *)
val validate_extension_point_exn
  :  name_of_ppx_rewriter:string
  -> loc:location
  -> tags:string list
  -> unit

val maybe_drop : Location.t -> Parsetree.expression -> Parsetree.structure
val if_am_test_runner : Location.t -> Parsetree.expression -> Parsetree.expression

(** Should be put around top-level effects that are used for testing only, but shouldn't
    be run otherwise. This tends to occur in ppx headers and footers. *)
val guard_toplevel_test_effects
  :  Location.t
  -> Parsetree.expression
  -> Parsetree.structure

(**/**)

val tags : (Parsetree.pattern, string list) Attribute.t
