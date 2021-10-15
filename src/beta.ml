open KNormal

(*  env: ある変数からそれに等しい変数への写像
    e: k正規化後の式
 *)
let g env e =
  match e with
  | Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | Neg s -> D.unimplemented "Neg"
  | Add (s1, s2) -> D.unimplemented "Add"
  | Sub (s1, s2) -> D.unimplemented "Sub"
  | FNeg s -> D.unimplemented "FNeg"
  | FAdd (s1, s2) -> D.unimplemented "FAdd"
  | FSub (s1, s2) -> D.unimplemented "FSub"
  | FMul (s1, s2) -> D.unimplemented "FMul"
  | FDiv (s1, s2) -> D.unimplemented "FDiv"
  | IfEq (s1, s2, e1, e2) -> D.unimplemented "IfEq"
  | IfLE (s1, s2, e1, e2) -> D.unimplemented "IfLE"
  | Let ((s1, t1), e1, e2) -> D.unimplemented "Let"
  | Var s -> D.unimplemented "Var"
  | LetRec (fundef, e) -> D.unimplemented "LetRec"
  | App (s, s_list) -> D.unimplemented "App"