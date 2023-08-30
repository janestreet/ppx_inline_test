(* Check that tests with the disabled tag are not run. *)

let%test (_ [@tags "disabled"]) = false
