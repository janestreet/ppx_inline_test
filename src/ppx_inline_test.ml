open StdLabels
open Ppx_core.Std
open Parsetree
open Ast_builder.Default

[@@@metaloc loc]

(* Generated code should depend on the environment in scope as little as
   possible.  E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the
   use of [=].  It is especially important to not use polymorphic comparisons, since we
   are moving more and more to code that doesn't have them in scope. *)


type maybe_drop =
  | Keep
  | Drop_with_deadcode
  | Drop

let maybe_drop_mode = ref Keep

let set_default_maybe_drop x = maybe_drop_mode := x

let () =
  Ppx_driver.add_arg "-inline-test-drop"
    (Arg.Unit (fun () -> maybe_drop_mode := Drop))
    ~doc:" Drop unit tests";
  Ppx_driver.add_arg "-inline-test-drop-with-deadcode"
    (Arg.Unit (fun () -> maybe_drop_mode := Drop_with_deadcode))
    ~doc:" Drop unit tests by wrapping them inside deadcode to prevent \
          unused variable warnings.";
;;

let maybe_drop loc code =
  match !maybe_drop_mode with
  | Keep               -> [%str let () = [%e code]]
  | Drop_with_deadcode -> [%str let () = if false then [%e code] else ()]
  | Drop               -> Attribute.explicitly_drop#expression code; [%str ]

let rec short_desc_of_expr ~max_len e =
  match e.pexp_desc with
  | Pexp_let (_, _, e) | Pexp_letmodule (_, _, e) ->
    short_desc_of_expr ~max_len e
  | _ ->
    let s = Pprintast.string_of_expression e in
    let res =
      if String.length s >= max_len then
        let s_short = String.sub s ~pos:0 ~len:(max_len - 5) in
        s_short ^ "[...]"
      else s
    in
    String.map res ~f:(function
      | '\n' -> ' '
      | c -> c)
;;

let descr ~(loc:Location.t) ?(inner_loc=loc) e_opt id_opt =
  let filename  = File_path.get_default_path loc                 in
  let line      = loc.loc_start.pos_lnum                         in
  let start_pos = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let end_pos   = inner_loc.Location.loc_end.pos_cnum - loc.loc_start.pos_bol in
  let descr =
    match id_opt, e_opt with
    | None, None -> ""
    | None, Some e -> ": <<" ^ short_desc_of_expr ~max_len:50 e ^ ">>"
    | Some id, _ -> ": " ^ id
  in
  (estring ~loc descr,
   estring ~loc filename,
   eint ~loc line,
   eint ~loc start_pos,
   eint ~loc end_pos)
;;

let apply_to_descr lid ~loc ?inner_loc e_opt id_opt more_arg =
  let descr, filename, line, start_pos, end_pos = descr ~loc ?inner_loc e_opt id_opt in
  let expr =
    eapply ~loc (evar ~loc ("Ppx_inline_test_lib.Runtime." ^ lid))
      [ descr; filename; line; start_pos; end_pos; more_arg ]
  in
  maybe_drop loc expr
;;

let enabled () =
  match !maybe_drop_mode, Ppx_inline_test_libname.get () with
  | Keep, None -> false
  | _          -> true
;;

let assert_enabled loc =
  if not (enabled ()) then
    Location.raise_errorf ~loc
      "ppx_inline_test: extension is disabled because the tests would be ignored \
       (the build system didn't pass -inline-test-lib)"
;;

let expand_test ~loc ~path:_ id e =
  assert_enabled loc;
  apply_to_descr "test" ~loc (Some e) id (pexp_fun ~loc "" None (punit ~loc) e)
;;

let expand_test_unit ~loc ~path:_ id e =
  assert_enabled loc;
  apply_to_descr "test_unit" ~loc (Some e) id (pexp_fun ~loc "" None (punit ~loc) e)
;;

let expand_test_module ~loc ~path:_ id m =
  assert_enabled loc;
  apply_to_descr "test_module" ~loc ~inner_loc:m.pmod_loc None id
    (pexp_fun ~loc "" None (punit ~loc)
       (pexp_letmodule ~loc (Located.mk ~loc "M")
          m
          (eunit ~loc)))
;;

module E = struct
  open Ast_pattern

  let opt_name_and_expr expr =
    pstr ((
      pstr_value nonrecursive (
        value_binding
          ~pat:(map (pstring __) ~f:(fun f x -> f (Some x)))
          ~expr ^:: nil)
      ||| map (pstr_eval expr nil) ~f:(fun f -> f None)
    ) ^:: nil)

  let test =
    Extension.V2.declare_inline "inline_test.test"
      Extension.Context.structure_item
      (opt_name_and_expr __)
      expand_test

  let test_unit =
    Extension.V2.declare_inline "inline_test.test_unit"
      Extension.Context.structure_item
      (opt_name_and_expr __)
      expand_test_unit

  let test_module =
    Extension.V2.declare_inline "inline_test.test_module"
      Extension.Context.structure_item
      (opt_name_and_expr (pexp_pack __))
      expand_test_module

  let all =
    [ test
    ; test_unit
    ; test_module
    ]
end

let () =
  Ppx_driver.register_transformation "inline-test"
    ~extensions:E.all
    ~impl:(fun st ->
      match Ppx_inline_test_libname.get () with
      | None -> st
      | Some libname ->
        let loc =
          match st with
          | [] -> Location.none
          | { pstr_loc = loc; _ } :: _ -> { loc with loc_end = loc.loc_start }
        in
        (* See comment in benchmark_accumulator.ml *)
        List.concat
          [ maybe_drop loc [%expr Ppx_inline_test_lib.Runtime.set_lib
                                    [%e estring ~loc libname]]
          ; st
          ; maybe_drop loc [%expr Ppx_inline_test_lib.Runtime.unset_lib
                                    [%e estring ~loc libname]]
          ]
    )
;;
