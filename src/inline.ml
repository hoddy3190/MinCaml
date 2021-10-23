open KNormal

(* インライン展開する関数の最大サイズ *)
let threshold = ref 0 (* Mainで-inlineオプションによりセットされる *)

let rec size = function [@warning "-4"]
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2)
  | Let(_, e1, e2) | LetRec({ body = e1 }, e2) -> 1 + size e1 + size e2
  | _ -> 1

(* env: keyが変数名、valが引数名のリストとLetRecの中身のペア *)
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
  | App (s, s_list) ->
    begin match M.find_opt s env with
    | Some (var_list, letrec_e1) ->
      (* letrec_e1の中の変数部分（var_list)をs_listで置き換える *)
      let env_for_alpha = List.fold_left2
      (fun acc (var, _) s ->
        M.add var s acc
      )
      M.empty
      var_list
      s_list
      in
      Alpha.g env_for_alpha e
    | None -> e
    end
