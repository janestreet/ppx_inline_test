let () =
  match Sys.getenv "ENABLE_FAILURES" with
  | exception Not_found -> ()
  | (_ : string) ->
    let module M = struct
      let%test _ = false
      let%test _ = raise Exit
      let%test_module "name" = (module struct
        let%test _ = false
        let%test _ = false
        let%test _ = raise Exit
        let%test_module _ = (module struct
          let () = raise Exit
        end)
      end)
      let%test_module _ = (module struct
        let () = raise Exit
      end)
    end in
    ()
;;
