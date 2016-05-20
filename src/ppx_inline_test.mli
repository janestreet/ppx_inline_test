open Ppx_core.Std

type maybe_drop =
  | Keep
  | Drop_with_deadcode
  | Drop

(** How to expand tests if no "-inline-test-drop*" command line flag is passed. *)
val set_default_maybe_drop : maybe_drop -> unit

val maybe_drop : Location.t -> Parsetree.expression -> Parsetree.structure

(**/**)

val opt_name_and_expr
  :  (Parsetree.expression, 'a, 'b) Ast_pattern.t
  -> (Parsetree.payload, name:string option -> tags:string list -> 'a, 'b)  Ast_pattern.t
