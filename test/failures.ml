(* Checking failures are reported properly, and make the overall test fail. *)

let%test _ = false
let%test "name1" = raise Exit

module%test Name2 = struct
  let%test _ = false
  let%test _ = false
  let%test _ = raise Exit

  module%test [@name "name3"] _ = struct
    let () = raise Exit
  end
end

module%test _ = struct
  let () = raise Exit
end

let x, y = "name", "4"
let%test [%name x ^ y] = false
