let%test_module _ =
  (module struct
    let%test_unit _ = if true then failwith "let percent test module did in fact run"
  end)
;;
