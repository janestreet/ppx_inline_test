open! Base

let show_init_result_exn init_result =
  match init_result with
  | Error err -> Printf.sprintf "<Error>\n%s" err |> Stdio.print_endline
  | Ok init_result ->
    (match init_result with
     | None -> Stdio.print_endline "<OK>"
     | Some help -> Printf.sprintf "<Help>\n%s" help |> Stdio.print_endline)
;;

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  Stdio.print_endline "(About to call [init] the first time)";
  args |> Ppx_inline_test_lib.init |> show_init_result_exn;
  Stdio.print_endline "(About to call [init] the second time)";
  args |> Ppx_inline_test_lib.init |> show_init_result_exn;
  Stdio.print_endline "(Was able to call [init] twice)"
;;
