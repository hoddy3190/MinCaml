exception UnexpectedType
let unexpected_type () = raise UnexpectedType

exception OccurCheckError of Type.t * Type.t
let occur_check_error t1 t2 = raise (OccurCheckError (t1, t2))

exception NotEqualType of Type.t * Type.t
let not_equal_type t1 t2 = raise (NotEqualType (t1, t2))

let () =
  Printexc.register_printer
    (function
      | OccurCheckError (t1, t2) -> Some (Printf.sprintf "OccurCheckError\nt1: %s\nt2: %s" (Type.show t1) (Type.show t2) )
      | NotEqualType (t1, t2) -> Some (Printf.sprintf "NotEqualType\nt1: %s\nt2: %s" (Type.show t1) (Type.show t2) )
      | _ -> None (* for other exceptions *)
    )
