(* OASIS_START *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
open OASISTypes;;
(* OASIS_STOP *)

let () =
  InternalInstallPlugin.lib_hook := fun (cs, bs, lib) ->
    (cs, bs, lib, [ "_build/as_ppx/ppx.byte" ])
;;

let () = setup ()
