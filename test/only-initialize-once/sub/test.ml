open Base

let%test_unit "running [init] from inside an inline test" =
  match
    Ppx_inline_test_lib.init
      [ "dummy"; "inline-test-runner"; "dummy"; "-list-partitions" ]
  with
  | Ok _ -> Stdio.print_endline "init succeeded"
  | Error err -> Stdio.print_endline err
;;
