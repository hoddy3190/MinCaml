open KNormal

(* インライン展開する関数の最大サイズ *)
let threshold = ref 0 (* Mainで-inlineオプションによりセットされる *)

let rec size = function [@warning "-4"]
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2)
  | Let(_, e1, e2) | LetRec({ body = e1 }, e2) -> 1 + size e1 + size e2
  | _ -> 1

let rec g env e =
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
  | IfEq (s1, s2, e1, e2) -> IfEq (s1, s2, g env e1, g env e2)
  | IfLE (s1, s2, e1, e2) -> IfLE (s1, s2, g env e1, g env e2)
  | Let ((s1, t1), e1, e2) -> Let ((s1, t1), g env e1, g env e2)
  | Var _ -> e
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2) ->
    let updated_env = if size e1 <= !threshold then
      M.add x (yts, e1) env
    else
      env
    in
    LetRec({ name = (x, t); args = yts; body = g env e1}, g env e2)
  | App _ ->  D.unimplemented "App"