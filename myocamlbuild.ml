(* OASIS_START *)
(* OASIS_STOP *)
# 3 "myocamlbuild.ml"

let dispatch = function
  | After_rules ->
    rule "workaround buggy tooling"
      ~dep:"%.cmxa"
      ~prod:"%.cmxs"
      ~insert:`top
      (fun env _ ->
         Cmd (S [ !Options.ocamlopt
                ; A "-shared"
                ; A "-linkall"
                ; A (env "%.cmxa")
                ; A "-o"
                ; A (env "%.cmxs")
                ]));
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
