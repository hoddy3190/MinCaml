open KNormal

let find x env =
    match M.find_opt x env with
    | Some var -> var
    | None -> x

(*  env: 変換前の変数名から変換後の変数名への写像
    e: k正規化後の式
    α変換後の式を返す
    Let, LetRecで値を束縛するところでは、新しいユニークな変数を作成しenvに格納する
    変数を使う式（Add, IfEqなど）はenvをもとに新しい変数に置き換える

    xという変数が複数箇所に使われていたとき、x -> x1, x -> x2,...に変換するkey, valの組み合わせをどうやってmapに格納するのか
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
    let s1' = Id.genid s1 in
    Let((s1', t1), g env e1, g (M.add s1 s1' env) e2)
  | Var s -> Var (find s env)
  | LetRec (fundef, e) ->
    (* 実装のやり方違うけどたぶんあってる？ *)
    let (s1, t1) = fundef.name in
    let s1' = Id.genid s1 in
    let updated_env_1 = M.add s1 s1' env in
    let args' = List.map (fun s_t -> let (s, t) = s_t in (Id.genid s, t)) fundef.args in
    let updated_env_2 = List.fold_left2 (fun acc s_t s'_t' ->
      let s, _ = s_t in
      let s', _ = s'_t' in
      M.add s s' acc
    ) updated_env_1 fundef.args args' in
    let fundef' = {
      name = (Id.genid s1, t1);
      args = args';
      body = g updated_env_2 fundef.body;
    } in
    LetRec (fundef', g updated_env_1 e)
  | App (s, s_list) -> App (find s env, List.map (fun s -> find s env) s_list)
