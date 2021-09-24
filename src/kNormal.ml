
type t = (* K正規化後の式 *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  (* | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list *)
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

(* 変数の型環境envとK正規化前の式とを受け取り、K正規化後の式とその型とを組にして返す *)
let g env expr =
  match expr with
  | Syntax.Ident s -> D.unimplemented "Ident"
  | Syntax.Int i -> Int i, Type.Int
  | Syntax.Float f -> Float f, Type.Float
  | Syntax.Bool b -> Int (if b then 1 else 0), Type.Int (* K正規化のついで *)
  | Syntax.Not e -> D.unimplemented "Not"
  | Syntax.Neg e -> D.unimplemented "Neg"
  | Syntax.Add (e1, e2) -> D.unimplemented "Add"
  | Syntax.Sub (e1, e2) -> D.unimplemented "Sub"
  | Syntax.Mul (e1, e2) -> D.unimplemented "Mul"
  | Syntax.Div (e1, e2) -> D.unimplemented "Div"
  | Syntax.FAdd (e1, e2) -> D.unimplemented "FAdd"
  | Syntax.FSub (e1, e2) -> D.unimplemented "FSub"
  | Syntax.FMul (e1, e2) -> D.unimplemented "FMul"
  | Syntax.FDiv (e1, e2) -> D.unimplemented "FDiv"
  | Syntax.Eq (e1, e2) -> D.unimplemented "Eq"
  | Syntax.Neq(e1, e2) -> D.unimplemented "Neq"
  | Syntax.Le (e1, e2) -> D.unimplemented "Le"
  | Syntax.Ge (e1, e2) -> D.unimplemented "Ge"
  | Syntax.Lt (e1, e2) -> D.unimplemented "Lt"
  | Syntax.Gt (e1, e2) -> D.unimplemented "Gt"
  | Syntax.If (e1, e2, e3) -> D.unimplemented "If"
  | Syntax.Let (e1, e2, e3) -> D.unimplemented "Let"
  | Syntax.LetRec (e1, e2, e3, e4) -> D.unimplemented "LetRec"
  | Syntax.App (e1, e2) -> D.unimplemented "App"
