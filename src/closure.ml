type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* クロージャ変換後の式。本家からコピー *)
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t }
type prog = Prog of fundef list * t


(* env: 型環境
   known: 自由変数がないとわかっていて、普通に呼び出せる関数の集合 *)
let rec g env known e =
  match e with
  | KNormal.Unit -> Unit
  | KNormal.Int i -> Int i
  | KNormal.Float f -> Float f
  | KNormal.Neg s -> Neg s
  | KNormal.Add (s1, s2) -> Add (s1, s2)
  | KNormal.Sub (s1, s2) -> Sub (s1, s2)
  | KNormal.FNeg s -> FNeg s
  | KNormal.FAdd (s1, s2) -> FAdd (s1, s2)
  | KNormal.FSub (s1, s2) -> FSub (s1, s2)
  | KNormal.FMul (s1, s2) -> FMul (s1, s2)
  | KNormal.FDiv (s1, s2) -> FDiv (s1, s2)
  | KNormal.IfEq (s1, s2, e1, e2) -> IfEq (s1, s2, g env known e1, g env known e2)
  | KNormal.IfLE (s1, s2, e1, e2) -> IfLE (s1, s2, g env known e1, g env known e2)
  | KNormal.Let ((s1, t1), e1, e2) -> D.unimplemented "Let"
  | KNormal.Var s -> Var s
  | KNormal.LetRec (fundef, e) -> D.unimplemented "LetRec"
  | KNormal.App (s, s_list) -> D.unimplemented "App"
