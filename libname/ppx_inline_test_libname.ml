open Ppxlib

let libname_and_partition = ref None

let () =
  Driver.add_arg
    "-inline-test-lib"
    (Arg.String
       (fun lib ->
         let p =
           match String.index lib ':' with
           | exception Not_found -> lib, None
           | i ->
             String.sub lib 0 i, Some (String.sub lib (i + 1) (String.length lib - i - 1))
         in
         libname_and_partition := Some p))
    ~doc:
      " A base name to use for generated identifiers (has to be globally unique in a \
       program). ppx_inline_test (and ppx_bench) are disabled unless this flag is \
       passed."
;;

let () =
  Driver.Cookies.add_simple_handler
    "library-name"
    Ast_pattern.(estring __)
    ~f:(function
      | None -> ()
      | Some lib -> libname_and_partition := Some (lib, None))
;;

let get () = !libname_and_partition
