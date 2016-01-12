let libname = ref None

let () =
  Ppx_driver.add_arg "-inline-test-lib" (Arg.String (fun s -> libname := Some s))
    ~doc:" A base name to use for generated identifiers \
          (has to be globally unique in a program).\
          ppx_inline_test (and ppx_bench) are disabled unless this flag is passed.";
;;

let get () = !libname
