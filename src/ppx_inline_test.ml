open StdLabels
open Ppx_core.Std
open Parsetree
open Ast_builder.Default
open Ppx_type_conv.Std

[@@@metaloc loc]

type test =
  | Test        of expression
  | Test_unit   of expression
  | Test_module of module_expr

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
    Extension.declare "inline_test.test" Extension.Context.structure_item
      (opt_name_and_expr __)
      (fun name e -> (name, Test e))

  let test_unit =
    Extension.declare "inline_test.test_unit" Extension.Context.structure_item
      (opt_name_and_expr __)
      (fun name e -> (name, Test_unit e))

  let test_module =
    Extension.declare "inline_test.test_module" Extension.Context.structure_item
      (opt_name_and_expr (pexp_pack __))
      (fun name x -> (name, Test_module x))

end

(* Generated code should depend on the environment in scope as little as
   possible.  E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the
   use of [=].  It is especially important to not use polymorphic comparisons, since we
   are moving more and more to code that doesn't have them in scope. *)


let libname = ref None

type maybe_drop =
  | Keep
  | Drop_with_deadcode
  | Drop

let maybe_drop_mode = ref Keep

let set_default_maybe_drop x = maybe_drop_mode := x

let () =
  Ppx_driver.add_arg "-inline-test-lib" (Arg.String (fun s -> libname := Some s))
    ~doc:" A base name to use for generated identifiers \
          (has to be globally unique in a program).\
          ppx_inline_test (and ppx_bench) are disabled unless this flag is passed.";
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
  | Drop               -> [%str ]

let libname () = !libname

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
    for i = 0 to String.length res -1 do
      if res.[i] = '\n' then
        res.[i] <- ' '
    done;
    res
;;

let descr ~(loc:Location.t) ?(inner_loc=loc) e_opt id_opt =
  let filename  = Type_conv_path.get_default_path loc in
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

let expand_test loc id test =
  match test with
  | Test e ->
    apply_to_descr "test" ~loc (Some e) id (pexp_fun ~loc "" None (punit ~loc) e)
  | Test_unit e ->
    apply_to_descr "test_unit" ~loc (Some e) id (pexp_fun ~loc "" None (punit ~loc) e)
  | Test_module m ->
    apply_to_descr "test_module" ~loc ~inner_loc:m.pmod_loc None id
      (pexp_fun ~loc "" None (punit ~loc)
         (pexp_letmodule ~loc (Located.mk ~loc "M")
            m
            (eunit ~loc)))
;;

let map ~expand = object
  inherit Ast_traverse.map as super

  method! structure st =
    let st = super#structure st in
    List.map st ~f:(fun st ->
      match st.pstr_desc with
      | Pstr_extension (ext, attrs) -> begin
          match Extension.convert E.[test; test_unit; test_module] ext with
          | None -> [st]
          | Some (id, test) ->
            assert_no_attributes attrs;
            expand st.pstr_loc id test
        end
      | _ -> [st]
    ) |> List.concat
end

let enabled = map ~expand:expand_test

let disabled =
  map ~expand:(fun loc _id _test ->
    Location.raise_errorf ~loc
      "ppx_inline_test: extension is disabled as no -inline-test-lib was given")
;;

let () =
  Ppx_driver.register_code_transformation
    ~name:"inline-test"
    ~intf:(fun sg -> sg)
    ~impl:(fun st ->
      match !maybe_drop_mode, libname() with
      | (Drop_with_deadcode | Drop), None -> enabled #structure st
      | Keep                       , None -> disabled#structure st
      | _, Some libname ->
        let st = enabled#structure st in
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
