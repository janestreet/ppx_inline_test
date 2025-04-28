(* Check that ignored attributes inside dropped tests do not trigger an error *)

module%test _ = struct
  [@@@attribute_not_handled_by_anything]
end
