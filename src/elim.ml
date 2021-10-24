open KNormal

(* 副作用の有無を判定
   副作用が本当にあるかどうかは決定不能なので、配列への書き込みか（現段階では未実装）、関数呼び出しがあったら副作用があると判定している
 *)
let rec effect e =
  match e with
  | Unit -> false
  | Int _ -> false
  | Float _ -> false
  | Neg _ -> false
  | Add _ -> false
  | Sub _ -> false
  | FNeg _ -> false
  | FAdd _ -> false
  | FSub _ -> false
  | FMul _ -> false
  | FDiv _ -> false
  | IfEq (_, _, e1, e2) -> effect e1 || effect e2
  | IfLE (_, _, e1, e2) -> effect e1 || effect e2
  | Let (_, e1, e2) -> effect e1 || effect e2
  | Var _ -> false
  | LetRec (_, e2) -> effect e2
  | App _ -> true

let rec f e =
  match e with
  | Unit -> e
  | Int _ -> e
  | Float _ -> e
  | Neg _ -> e
  | Add _ -> e
  | Sub _ -> e
  | FNeg _ -> e
  | FAdd _ -> e
  | FSub _ -> e
  | FMul _ -> e
  | FDiv _ -> e
  | IfEq (s1, s2, e1, e2) -> IfEq (s1, s2, f e1, f e2)
  | IfLE (s1, s2, e1, e2) -> IfLE (s1, s2, f e1, f e2)
  | Let ((s1, t1), e1, e2) -> D.unimplemented "Let"
  | Var _ -> e
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2) -> D.unimplemented "LetRec"
  | App _ -> e
