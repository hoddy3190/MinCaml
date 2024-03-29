type t = (* MinCamlの構文を表現するデータ型 *)
  (* | Unit  *)
  | Ident of string
  | Int of int
  | Float of float
  | Bool of bool
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  (* | FNeg of t *)
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | Neq of t * t
  | Le of t * t
  | Ge of t * t
  | Lt of t * t
  | Gt of t * t
  | If of t * t * t
  | Let of t * t * t
  | LetRec of t * (t list) * t * t
  | App of t * t list
[@@deriving show]
  (*
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t *)
(* and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t } *)
