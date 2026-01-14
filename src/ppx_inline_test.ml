open Base
open Ppxlib
open Ast_builder.Default

(* Generated code should depend on the environment in scope as little as possible. E.g.
   rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=]. It is
   especially important to not use polymorphic comparisons, since we are moving more and
   more to code that doesn't have them in scope. *)

type maybe_drop =
  | Keep
  | Drop_with_deadcode
  | Drop

let maybe_drop_mode = ref Keep
let set_default_maybe_drop x = maybe_drop_mode := x
let allow_let_test_module = ref false
let allow_let_test_module_flag = "-inline-test-allow-let-test-module"

let () =
  Driver.add_arg
    "-inline-test-drop"
    (Unit (fun () -> maybe_drop_mode := Drop))
    ~doc:" Drop unit tests";
  Driver.add_arg
    "-inline-test-drop-with-deadcode"
    (Unit (fun () -> maybe_drop_mode := Drop_with_deadcode))
    ~doc:
      " Drop unit tests by wrapping them inside deadcode to prevent unused variable \
       warnings.";
  Driver.add_arg
    allow_let_test_module_flag
    (Set allow_let_test_module)
    ~doc:" Allow [let%test_module]; otherwise, require newer form [module%test]."
;;

let () =
  Driver.Cookies.add_simple_handler
    "inline-test"
    Ast_pattern.(pexp_ident (lident __'))
    ~f:(function
      | None -> ()
      | Some id ->
        (match id.txt with
         | "drop" -> maybe_drop_mode := Drop
         | "drop_with_deadcode" -> maybe_drop_mode := Drop_with_deadcode
         | s ->
           Location.raise_errorf
             ~loc:id.loc
             "invalid 'inline-test' cookie (%s), expected one of: drop, \
              drop_with_deadcode"
             s))
;;

(* Same as above, but for the Dune setting *)
let () =
  Driver.Cookies.add_simple_handler
    "inline_tests"
    Ast_pattern.(estring __')
    ~f:(function
      | None -> ()
      | Some id ->
        (match id.txt with
         | "enabled" -> maybe_drop_mode := Keep
         | "disabled" -> maybe_drop_mode := Drop
         | "ignored" -> maybe_drop_mode := Drop_with_deadcode
         | s ->
           Location.raise_errorf
             ~loc:id.loc
             "invalid 'inline_tests' cookie (%s), expected one of: enabled, disabled or \
              ignored"
             s))
;;

let maybe_drop loc code =
  match !maybe_drop_mode with
  | Keep -> [%str let () = [%e code]]
  | Drop_with_deadcode -> [%str let () = if false then [%e code] else ()]
  | Drop ->
    Attribute.explicitly_drop#expression code;
    [%str]
;;

let if_am_test_runner loc expr =
  [%expr if Ppx_inline_test_lib.am_test_runner () then [%e expr]]
;;

let guard_toplevel_test_effects loc expr = maybe_drop loc (if_am_test_runner loc expr)

let rec short_desc_of_expr ~max_len e =
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree e.pexp_desc ~loc:e.pexp_loc with
  | Pexp_let (_, _, _, e) | Pexp_letmodule (_, _, e) -> short_desc_of_expr ~max_len e
  | _ ->
    let s = Pprintast.string_of_expression e in
    let res =
      if String.length s >= max_len
      then (
        let s_short = String.sub s ~pos:0 ~len:(max_len - 5) in
        s_short ^ "[...]")
      else s
    in
    String.map res ~f:(function
      | '\n' -> ' '
      | c -> c)
;;

let descr ~(loc : Location.t) ?(inner_loc = loc) e_opt id_opt =
  let filename = loc.loc_start.pos_fname in
  let line = loc.loc_start.pos_lnum in
  let start_pos = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let end_pos = inner_loc.Location.loc_end.pos_cnum - loc.loc_start.pos_bol in
  let descr =
    match id_opt with
    | `Literal id -> estring ~loc id
    | `Expr e -> e
    | `None ->
      estring
        ~loc
        (match e_opt with
         | None -> ""
         | Some e -> "<<" ^ short_desc_of_expr ~max_len:50 e ^ ">>")
  in
  ( pexp_lazy ~loc descr
  , estring ~loc filename
  , eint ~loc line
  , eint ~loc start_pos
  , eint ~loc end_pos )
;;

let apply_to_descr lid ~loc ?inner_loc e_opt id_opt tags more_arg =
  let descr, filename, line, start_pos, end_pos = descr ~loc ?inner_loc e_opt id_opt in
  let expr =
    pexp_apply
      ~loc
      (evar ~loc ("Ppx_inline_test_lib." ^ lid))
      [ Labelled "config", [%expr (module Inline_test_config)]
      ; Labelled "descr", descr
      ; Labelled "tags", elist ~loc (List.map ~f:(estring ~loc) tags)
      ; Labelled "filename", filename
      ; Labelled "line_number", line
      ; Labelled "start_pos", start_pos
      ; Labelled "end_pos", end_pos
      ; Nolabel, more_arg
      ]
  in
  maybe_drop loc expr
;;

let can_use_test_extensions () =
  match !maybe_drop_mode, Ppx_inline_test_libname.get () with
  | Keep, None -> false
  | (Drop | Drop_with_deadcode), _ | _, Some _ -> true
;;

(* Set to [true] when we see a [let%test] or [let%expect_test] etc extension. *)
module Has_tests =
  Driver.Create_file_property
    (struct
      let name = "ppx_inline_test.has_tests"
    end)
    (Bool)

let all_tags =
  [ "no-js"
  ; "js-only"
  ; "no-wasm"
  ; "wasm-only"
  ; "64-bits-only"
  ; "32-bits-only"
  ; "fast-flambda"
  ; "fast-flambda2"
  ; "x-library-inlining-sensitive"
  ; "not-on-el7"
  ; "not-on-el8"
  ; "disabled"
  ; "runtime5-only"
  ; "runtime4-only"
  ]
;;

let validate_tag tag =
  if not (List.mem all_tags tag ~equal:String.equal)
  then Error (Spellcheck.spellcheck all_tags tag)
  else Ok ()
;;

let validate_extension_point_exn ~name_of_ppx_rewriter ~loc ~tags =
  Has_tests.set true;
  if not (can_use_test_extensions ())
  then
    Location.raise_errorf
      ~loc
      "%s: extension is disabled because the tests would be ignored.\n\
       (The build system didn't pass -inline-test-lib. With Dune, this usually happens \
       for one of two reasons:\n\
       (1) Tests were written in files that are part of an executables stanza, but only \
       library stanzas support inline tests.\n\
       (2) Tests were written in a parametrized library, but no [instantiate_parameters] \
       was given in the [inline_tests] clause.)"
      name_of_ppx_rewriter;
  List.iter tags ~f:(fun tag ->
    match validate_tag tag with
    | Ok () -> ()
    | Error hint ->
      let hint =
        match hint with
        | None -> ""
        | Some hint -> "\n" ^ hint
      in
      Location.raise_errorf
        ~loc
        "%s: %S is not a valid tag for inline tests.%s"
        name_of_ppx_rewriter
        tag
        hint)
;;

let name_of_ppx_rewriter = "ppx_inline_test"

let expand_let_test ~loc ~path:_ ~name:id ~tags e =
  let loc = { loc with loc_ghost = true } in
  validate_extension_point_exn ~name_of_ppx_rewriter ~loc ~tags;
  apply_to_descr "test" ~loc (Some e) id tags [%expr fun () -> [%e e]]
;;

let expand_test_unit ~loc ~path:_ ~name:id ~tags e =
  let loc = { loc with loc_ghost = true } in
  validate_extension_point_exn ~name_of_ppx_rewriter ~loc ~tags;
  (* The "; ()" bit is there to breaks tail call optimization, for better backtraces. *)
  apply_to_descr
    "test_unit"
    ~loc
    (Some e)
    id
    tags
    [%expr
      fun () ->
        [%e e];
        ()]
;;

let expand_test_module ~is_let_test_module ~loc ~path:_ ~name:id ~tags m =
  let loc = { loc with loc_ghost = true } in
  if is_let_test_module && not !allow_let_test_module
  then
    Location.raise_errorf
      ~loc
      "Convert [%s] to [%s] or pass [%s] to ppx driver"
      "let%test_module"
      "module%test"
      allow_let_test_module_flag;
  validate_extension_point_exn ~name_of_ppx_rewriter ~loc ~tags;
  apply_to_descr
    "test_module"
    ~loc
    ~inner_loc:m.pmod_loc
    None
    id
    tags
    (pexp_fun
       ~loc
       Nolabel
       None
       (punit ~loc)
       (pexp_letmodule ~loc (Located.mk ~loc (Some "M")) m (eunit ~loc)))
;;

let expand_test ~loc ~path variant =
  match variant with
  | `Let (name, tags, e) -> expand_let_test ~loc ~path ~name ~tags e
  | `Module (name, tags, m) ->
    expand_test_module ~is_let_test_module:false ~loc ~path ~name ~tags m
;;

module E = struct
  open Ast_pattern

  let make_tags context =
    Attribute.declare
      "tags"
      context
      (single_expr_payload
         (pexp_tuple (many (estring __)) ||| map (estring __) ~f:(fun f x -> f [ x ])))
      (fun x -> x)
  ;;

  let pattern_tags = make_tags Attribute.Context.pattern
  let module_tags = make_tags Attribute.Context.module_binding

  let list_of_option = function
    | None -> []
    | Some x -> x
  ;;

  let opt_name () =
    map (pstring __) ~f:(fun f x -> f (`Literal x))
    ||| map ppat_any ~f:(fun f -> f `None)
    ||| map
          (ppat_extension
             (extension (cst ~to_string:Fn.id "name") (single_expr_payload __)))
          ~f:(fun f e -> f (`Expr e))
  ;;

  let opt_name_and_expr expr =
    pstr
      (pstr_value
         nonrecursive
         (value_binding
            ~pat:
              (map
                 (Attribute.pattern pattern_tags (opt_name ()))
                 ~f:(fun f attributes name_opt ->
                   f ~name:name_opt ~tags:(list_of_option attributes)))
            ~expr
          ^:: nil)
       ^:: nil)
  ;;

  let module_name_pattern pat =
    Ast_pattern.of_func (fun ctx loc mb k ->
      let name_attrs, other_attrs =
        List.partition_map mb.pmb_attributes ~f:(fun attr ->
          match attr with
          | { attr_name = { txt = "name"; loc = _ }
            ; attr_payload =
                PStr
                  [%str
                    [%e? { pexp_desc = Pexp_constant (Pconst_string (name, _, _)); _ }]]
            ; attr_loc = _
            } -> First (attr, name)
          | _ -> Second attr)
      in
      match name_attrs with
      | [] -> Ast_pattern.to_func pat ctx loc mb (k None)
      | [ (attr, name) ] ->
        Attribute.mark_as_handled_manually attr;
        Ast_pattern.to_func
          pat
          ctx
          loc
          { mb with pmb_attributes = other_attrs }
          (k (Some name))
      | _ :: _ :: _ -> Location.raise_errorf ~loc "duplicate @name attribute")
  ;;

  let module_name_and_expr expr =
    pstr
      (pstr_module
         (module_binding ~name:__' ~expr
          |> module_name_pattern
          |> Attribute.pattern module_tags
          |> map0' ~f:Fn.id
          |> map ~f:(fun f loc tags attr_name bind_name m ->
            let tags = list_of_option tags in
            let name =
              match attr_name, bind_name.txt with
              | None, None -> `None
              | Some name, None | None, Some name -> `Literal name
              | Some attr_name, Some bind_name ->
                Location.raise_errorf
                  ~loc
                  "multiple names; use one of:\n\
                  \  [module%%test %s =], or\n\
                  \  [module%%test [@name %S] _ =],\n\
                   but not both."
                  bind_name
                  attr_name
            in
            f ~name ~tags m))
       ^:: nil)
  ;;

  let let_or_module_name_and_expr =
    let let_pattern =
      map (opt_name_and_expr __) ~f:(fun f ~name ~tags e -> f (`Let (name, tags, e)))
    in
    let module_pattern =
      map (module_name_and_expr __) ~f:(fun f ~name ~tags m ->
        f (`Module (name, tags, m)))
    in
    let_pattern ||| module_pattern
  ;;

  let test =
    Extension.declare_inline
      "inline_test.test"
      Extension.Context.structure_item
      let_or_module_name_and_expr
      expand_test
  ;;

  let test_unit =
    Extension.declare_inline
      "inline_test.test_unit"
      Extension.Context.structure_item
      (opt_name_and_expr __)
      expand_test_unit
  ;;

  let test_module =
    Extension.declare_inline
      "inline_test.test_module"
      Extension.Context.structure_item
      (opt_name_and_expr (pexp_pack __))
      (expand_test_module ~is_let_test_module:true)
  ;;

  let all = [ test; test_unit; test_module ]
end

let tags = E.pattern_tags

let () =
  Driver.V2.register_transformation
    "inline-test"
    ~extensions:E.all
    ~enclose_impl:(fun ctxt loc ->
      match loc, Ppx_inline_test_libname.get () with
      | None, _ | _, None -> [], []
      | Some loc, Some (libname, partition_opt) ->
        let partition =
          match partition_opt with
          | None -> Stdlib.Filename.basename (Expansion_context.Base.input_name ctxt)
          | Some p -> p
        in
        let loc = { loc with loc_ghost = true } in
        (* See comment in benchmark_accumulator.ml *)
        let header =
          let loc = { loc with loc_end = loc.loc_start } in
          maybe_drop
            loc
            [%expr
              Ppx_inline_test_lib.set_lib_and_partition
                [%e estring ~loc libname]
                [%e estring ~loc partition]]
        and footer =
          let loc = { loc with loc_start = loc.loc_end } in
          maybe_drop loc [%expr Ppx_inline_test_lib.unset_lib [%e estring ~loc libname]]
        in
        header, footer)
;;
