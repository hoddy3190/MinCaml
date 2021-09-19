open Syntax

(* env: 型環境（変数の名前から、その型への写像） *)
let rec g env (expr:t) =
  match expr with
  | Ident string -> D.unimplemented "Ident"
  | Int int -> D.unimplemented "Int"
  | Float float -> D.unimplemented "Float"
  | Bool bool -> D.unimplemented "Bool"
  | Not t -> D.unimplemented "Not"
  | Neg t -> D.unimplemented "Neg"
  | Add (t, t2) -> D.unimplemented "Add"
  | Sub (t, t2) -> D.unimplemented "Sub"
  | Mul (t, t2) -> D.unimplemented "Mul"
  | Div (t, t2) -> D.unimplemented "Div"
  | FAdd (t, t2) -> D.unimplemented "FAdd"
  | FSub (t, t2) -> D.unimplemented "FSub"
  | FMul (t, t2) -> D.unimplemented "FMul"
  | FDiv (t, t2) -> D.unimplemented "FDiv"
  | Eq (t, t2) -> D.unimplemented "Eq"
  | Neq (t, t2) -> D.unimplemented "Neq"
  | Le (t, t2) -> D.unimplemented "Le"
  | Ge (t, t2) -> D.unimplemented "Ge"
  | Lt (t, t2) -> D.unimplemented "Lt"
  | Gt (t, t2) -> D.unimplemented "Gt"
  | If (t, t2, t3) -> D.unimplemented "If"
  | Let (t, t2, t3) -> D.unimplemented "Let"
  | LetRec (t, t2, t3, t4) -> D.unimplemented "LetRec"
