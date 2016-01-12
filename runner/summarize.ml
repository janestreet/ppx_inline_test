open Ppx_inline_test_lib

let () = Runtime.Test_result.record (Runtime.summarize ())
