let%test ("alloc-test-ok" [@tags "x-library-inlining-sensitive"]) = true

(* Let's just pretend we have a test, say an alloc test, that only works with inlining,
   and is currently broken. *)
let%test ("alloc-test-fail" [@tags "x-library-inlining-sensitive"]) = false

module%test [@name "alloc-test-module2"] _ = struct
  let%test _ = true
  let%test (_ [@tags "x-library-inlining-sensitive"]) = true
end

module%test [@name "alloc-test-module"] [@tags "x-library-inlining-sensitive"] _ = struct
  let%test "ok" = true
  let%test "fail" = false
end

module%test [@name "early-cutoff-module"] [@tags "x-library-inlining-sensitive"] _ =
struct
  (* the toplevel of this module should not even be run when we aren't running the
     inlining-sensitive tests. *)
  let () = assert false
end
