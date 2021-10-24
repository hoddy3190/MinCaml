open KNormal

(* env : 変数の名前から値への写像
         値は整数、浮動小数、式など
 *)
let rec g env e =
  match e with
  | Unit -> e
  | Int _ -> e
  | Float _ -> e
  | Neg s ->
    begin [@warning "-4"] match M.find_opt s env with
    | Some (Int i) -> Int (-i)
    | _ -> e
    end
  | Add (s1, s2) ->
    begin [@warning "-4"] match M.find_opt s1 env, M.find_opt s2 env with
    | Some (Int i1), Some (Int i2) -> Int(i1 + i2)
    (* 少なくとも一方がNone、つまりenvに登録されていないとき、畳み込みはせずにそのまま返す *)
    | _ -> e
    end
  | Sub (s1, s2) ->
    begin [@warning "-4"] match M.find_opt s1 env, M.find_opt s2 env with
    | Some (Int i1), Some (Int i2) -> Int(i1 - i2)
    | _ -> e
    end
  | FNeg s ->
    begin [@warning "-4"] match M.find_opt s env with
    | Some (Float f) -> Float (-.f)
    | _ -> e
    end
  | FAdd (s1, s2) ->
    begin [@warning "-4"] match M.find_opt s1 env, M.find_opt s2 env with
    | Some (Float f1), Some (Float f2) -> Float(f1 +. f2)
    | _ -> e
    end
  | FSub (s1, s2) ->
    begin [@warning "-4"] match M.find_opt s1 env, M.find_opt s2 env with
    | Some (Float f1), Some (Float f2) -> Float(f1 -. f2)
    | _ -> e
    end
  | FMul (s1, s2) ->
    begin [@warning "-4"] match M.find_opt s1 env, M.find_opt s2 env with
    | Some (Float f1), Some (Float f2) -> Float(f1 *. f2)
    | _ -> e
    end
  | FDiv (s1, s2) ->
    begin [@warning "-4"] match M.find_opt s1 env, M.find_opt s2 env with
    | Some (Float f1), Some (Float f2) -> Float(f1 /. f2)
    | _ -> e
    end
  | IfEq (s1, s2, e1, e2) -> D.unimplemented "IfEq"
  | IfLE (s1, s2, e1, e2) -> D.unimplemented "IfLE"
  | Let ((s1, t1), e1, e2) -> D.unimplemented "Let"
  | Var s -> D.unimplemented "Var"
  | LetRec ({ name = xt; args = yts; body = e1 }, e2) -> D.unimplemented "LetRec"
  | App (s, s_list) -> D.unimplemented "App"