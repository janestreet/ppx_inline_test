let () =
  Libmain.set_main (fun () -> Ppx_inline_test_lib.Runtime.exit ())
