open KNormal

let find x env =
    match M.find_opt x env with
    | Some var -> var
    | None -> x

(*  env: ある変数からそれに等しい変数への写像
    e: k正規化後の式
 *)
let rec g env e =
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
  | IfEq (s1, s2, e1, e2) -> IfEq (find s1 env, find s2 env, g env e1, g env e2)
  | IfLE (s1, s2, e1, e2) -> IfLE (find s1 env, find s2 env, g env e1, g env e2)
  | Let ((s1, t1), e1, e2) ->
    begin [@warning "-4"] match g env e1 with
    | Var s1' -> g (M.add s1 s1' env) e2
    | e1' -> let e2' = g env e2 in Let ((s1, t1), e1', e2')
    end
  | Var s -> Var (find s env)
  | LetRec ({ name = xt; args = yts; body = e1 }, e2) ->
    LetRec({ name = xt; args = yts; body = g env e1 }, g env e2)
  | App (s, s_list) -> App (find s env, List.map (fun s -> find s env) s_list)
