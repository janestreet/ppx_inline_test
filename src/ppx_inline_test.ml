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

let apply_to_descr lid ~loc ?inner_loc e_opt id_opt tags more_arg =
  let descr, filename, line, start_pos, end_pos = descr ~loc ?inner_loc e_opt id_opt in
  let expr =
    pexp_apply ~loc (evar ~loc ("Ppx_inline_test_lib.Runtime." ^ lid))
      [ "config", [%expr (module Inline_test_config)]
      ; "descr", descr
      ; "tags", elist ~loc (List.map ~f:(estring ~loc) tags)
      ; "filename", filename
      ; "line_number", line
      ; "start_pos", start_pos
      ; "end_pos", end_pos
      ; "", more_arg ]
  in
  maybe_drop loc expr
;;

let enabled () =
  match !maybe_drop_mode, Ppx_inline_test_libname.get () with
  | Keep, None -> false
  | _          -> true
;;

let all_tags = ["no-js"]

let check_exn ~loc ~tags =
  if not (enabled ()) then
    Location.raise_errorf ~loc
      "ppx_inline_test: extension is disabled because the tests would be ignored \
       (the build system didn't pass -inline-test-lib)";
  List.iter tags ~f:(fun tag ->
    if not (List.mem tag ~set:all_tags)
    then
      let hint = match Ppx_core.Spellcheck.spellcheck all_tags tag with
        | None -> ""
        | Some hint -> "\n"^hint
      in
      Location.raise_errorf ~loc
        "ppx_inline_test: %S is not a valid tag for inline tests.%s" tag hint
  )
;;

let expand_test ~loc ~path:_ ~name:id ~tags e =
  check_exn ~loc ~tags;
  apply_to_descr "test" ~loc (Some e) id tags (pexp_fun ~loc "" None (punit ~loc) e)
;;

let expand_test_unit ~loc ~path:_ ~name:id ~tags e =
  check_exn ~loc ~tags;
  apply_to_descr "test_unit" ~loc (Some e) id tags (pexp_fun ~loc "" None (punit ~loc) e)
;;

let expand_test_module ~loc ~path:_ ~name:id ~tags m =
  check_exn ~loc ~tags;
  apply_to_descr "test_module" ~loc ~inner_loc:m.pmod_loc None id tags
    (pexp_fun ~loc "" None (punit ~loc)
       (pexp_letmodule ~loc (Located.mk ~loc "M")
          m
          (eunit ~loc)))
;;

module E = struct
  open Ast_pattern

  let tags =
    Attribute.declare
      "tags"
      Attribute.Context.pattern
      (single_expr_payload (
         pexp_tuple (many (estring __))
         |||  map (estring __) ~f:(fun f x -> f [x])))
      (fun x -> x)

  let list_of_option = function
    | None -> []
    | Some x -> x

  let opt_name () =
         map (pstring __) ~f:(fun f x -> f (Some x))
     ||| map ppat_any     ~f:(fun f   -> f None)
     ||| map (ppat_var (string "__")) ~f:(fun f   -> f None)

  let opt_name_and_expr expr =
    pstr ((
      pstr_value nonrecursive (
        (value_binding
           ~pat:(
             map
               (Attribute.pattern tags (opt_name ()))
               ~f:(fun f attributes name_opt ->
                 f ~name:name_opt ~tags:(list_of_option attributes)))
           ~expr)
        ^:: nil)
      ||| map
            (pstr_eval expr nil)
            ~f:(fun f -> f ~name:None ~tags:[])
    ) ^:: nil)

  let test =
    Extension.declare_inline "inline_test.test"
      Extension.Context.structure_item
      (opt_name_and_expr __)
      expand_test

  let test_unit =
    Extension.declare_inline "inline_test.test_unit"
      Extension.Context.structure_item
      (opt_name_and_expr __)
      expand_test_unit

  let test_module =
    Extension.declare_inline "inline_test.test_module"
      Extension.Context.structure_item
      (opt_name_and_expr  (pexp_pack __))
      expand_test_module

  let all =
    [ test
    ; test_unit
    ; test_module
    ]
end

let opt_name_and_expr = E.opt_name_and_expr

let () =
  Ppx_driver.register_transformation "inline-test"
    ~extensions:E.all
    ~enclose_impl:(fun loc ->
      match loc, Ppx_inline_test_libname.get () with
      | None, _ | _, None -> ([], [])
      | Some loc, Some (libname, partition) ->
        (* See comment in benchmark_accumulator.ml *)
        let header =
          let loc = { loc with loc_end = loc.loc_start } in
          maybe_drop loc [%expr Ppx_inline_test_lib.Runtime.set_lib_and_partition
                                  [%e estring ~loc libname] [%e estring ~loc partition]]
        and footer =
          let loc = { loc with loc_start = loc.loc_end } in
          maybe_drop loc [%expr Ppx_inline_test_lib.Runtime.unset_lib
                                  [%e estring ~loc libname]]
        in
        (header, footer)
    )
;;
