#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_inline_test"
  [ oasis_lib "inline_test_config"
  ; oasis_lib "ppx_inline_test"
  ; oasis_lib "ppx_inline_test_drop"
  ; oasis_lib "ppx_inline_test_lib"
  ; oasis_lib "ppx_inline_test_libname"
  ; oasis_obj "ppx_inline_test_runner"
  ; file "META" ~section:"lib"
  ; oasis_exe "ppx" ~dest:"../lib/ppx_inline_test/ppx"
  ]
