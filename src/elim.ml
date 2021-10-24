open KNormal

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
