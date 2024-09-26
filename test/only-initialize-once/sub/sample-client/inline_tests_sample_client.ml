open! Base

let show_init_result init_result =
  match init_result with
  | Error err -> Printf.sprintf "<Error>\n%s" err |> Stdio.print_endline
  | Ok init_result ->
    (match init_result with
     | None -> Stdio.print_endline "<OK>"
     | Some help -> Printf.sprintf "<Help>\n%s" help |> Stdio.print_endline)
;;

let show_using_color () =
  Stdio.print_endline "Checking whether the inline tests should [use_color]";
  match Ppx_inline_test_lib.use_color () with
  | use_color -> Printf.sprintf "<OK>\n[use_color] is %b" use_color |> Stdio.print_endline
  | exception exn ->
    Printf.sprintf "<Error>\n%s" (Exn.to_string exn) |> Stdio.print_endline
;;

let rec extract_flag args ~flag =
  match args with
  | [] | [ _ ] -> args, None
  | flag' :: arg :: args ->
    if String.equal flag' flag
    then args, Some arg
    else (
      let tl, res = extract_flag (arg :: args) ~flag in
      flag' :: tl, res)
;;

let () =
  let args = Array.to_list (Sys.get_argv ()) in
  let args, show_using_color_flag = extract_flag args ~flag:"-show-using-color" in
  match show_using_color_flag with
  | Some "before" -> show_using_color ()
  | _ ->
    Stdio.print_endline "(About to call [init] the first time)";
    args |> Ppx_inline_test_lib.init |> show_init_result;
    (match show_using_color_flag with
     | Some "after" -> show_using_color ()
     | _ ->
       Stdio.print_endline "(About to call [init] the second time)";
       args |> Ppx_inline_test_lib.init |> show_init_result;
       Stdio.print_endline "(Was able to call [init] twice)")
;;
