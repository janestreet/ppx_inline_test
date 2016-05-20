let libname = ref None

let () =
  Ppx_driver.add_arg "-inline-test-lib" (Arg.String (fun s -> libname := Some s))
    ~doc:" A base name to use for generated identifiers \
          (has to be globally unique in a program).\
          ppx_inline_test (and ppx_bench) are disabled unless this flag is passed.";
;;

let get () =
  match !libname with
  | None -> None
  | Some lib ->
    match String.index lib ':' with
    | exception Not_found -> Some (lib, "")
    | i -> Some (String.sub lib 0 i, String.sub lib (i + 1) (String.length lib - i - 1))
