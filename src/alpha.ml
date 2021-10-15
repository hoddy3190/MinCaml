open KNormal

let find x env =
    match M.find_opt x env with
    | Some var -> var
    | None -> x

(*  env: 変換前の変数名から変換後の変数名への写像
    e: k正規化後の式
 *)
let g env e =
  match e with
  | Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | Neg s -> Neg (find s env)
  | Add (s1, s2) -> Add (find s1 env, find s2 env)
  | Sub (s1, s2) -> Sub (find s1 env, find s2 env)
  | FNeg s -> FNeg (find s env)
  | FAdd (s1, s2) -> FAdd (find s1 env, find s2 env)
  | FSub (s1, s2) -> FSub (find s1 env, find s2 env)
  | FMul (s1, s2) -> FMul (find s1 env, find s2 env)
  | FDiv (s1, s2) -> FDiv (find s1 env, find s2 env)
  | IfEq (s1, s2, e1, e2) -> D.unimplemented "IfEq"
  | IfLE (s1, s2, e1, e2) -> D.unimplemented "IfLE"
  | Let ((s1, t1), e1, e2) -> D.unimplemented "Let"
  | Var s -> Var (find s env)
  | LetRec (fundef, e) -> D.unimplemented "LetRec"
  | App (s, s_list) -> D.unimplemented "App"
