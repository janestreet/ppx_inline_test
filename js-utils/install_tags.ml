let package_name = "ppx_inline_test"

let sections =
  [ ("lib",
    [ ("built_lib_inline_test_config", None)
    ; ("built_lib_ppx_inline_test", None)
    ; ("built_lib_ppx_inline_test_drop", None)
    ; ("built_lib_ppx_inline_test_lib", None)
    ; ("built_lib_ppx_inline_test_libname", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ppx", Some "../lib/ppx_inline_test/ppx")
    ],
    [])
  ]
