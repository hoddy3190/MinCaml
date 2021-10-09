open Err

type t = (* K正規化後の式 *)
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
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  (* | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list *)
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let insert_let (k_normal_e, t) k =
    begin [@warning "-4"] match k_normal_e with
    (* Varだけ特別視する必要ある？ *)
    | Var s -> k s
    | _ ->
      let new_var_name = Id.gentmp t in
      let e', t' = k new_var_name in
      Let ((new_var_name, t), k_normal_e, e'), t'
    end

(* 変数の型環境envとK正規化前の式とを受け取り、K正規化後の式とその型とを組にして返す *)
let rec g env expr =
  match expr with
  | Syntax.Ident s ->
    if M.mem s env then
        Var s, M.find s env
    else
        not_found_in_type_env ()
  | Syntax.Int i -> Int i, Type.Int
  | Syntax.Float f -> Float f, Type.Float
  | Syntax.Bool b -> Int (if b then 1 else 0), Type.Int (* K正規化のついで *)
  | Syntax.Not e -> g env (Syntax.If (e, Syntax.Bool false, Syntax.Bool true))
    (* なぜこうじゃない？
    let k_e, _ = g env e in
    let [@warning "-4"] i = match k_e with Int i -> i | _ -> unexpected_expr () in
    Int (if i = 1 then 0 else 1), Type.Int *)
  | Syntax.Neg e ->
    insert_let (g env e)
      (fun var -> Neg var, Type.Int)
  | Syntax.Add (e1, e2) ->
    (*
        Add (Add 1 2) (Add 3 4)
        ->
        let tmp1 = 1 + 2 in
        let tmp2 = 3 + 4 in
        tmp1 + tmp2

        g env (Add (Int 1) (Int 2))
        -> let t1: Type.Int = 1 in
        -> let t2: Type.Int = 2 in
        -> t1 + t2
    *)
    insert_let (g env e1)
      (fun var1 ->
        insert_let (g env e2)
          (fun var2 ->
            Add (var1, var2), Type.Int
          )
      )
  | Syntax.Sub (e1, e2) ->
    insert_let (g env e1)
      (fun var1 ->
        insert_let (g env e2)
          (fun var2 ->
            Sub (var1, var2), Type.Int
          )
      )
  | Syntax.Mul (e1, e2) ->
    (* TODO: テスト *)
    begin [@warning "-4"] match g env e2 with
    | Int i, _ ->
      let replaced_syntax =
        List.fold_left (fun e1 acc ->
          Syntax.Add (e1, acc)
        )
        (Syntax.Int 0)
        (UtilList.repeat e1 i)
      in
      g env replaced_syntax
    | _ -> unexpected_type ()
    end
  | Syntax.Div (e1, e2) -> D.unimplemented "Div"
  | Syntax.FAdd (e1, e2) ->
    insert_let (g env e1)
      (fun var1 ->
        insert_let (g env e2)
          (fun var2 ->
            FAdd (var1, var2), Type.Float
          )
      )
  | Syntax.FSub (e1, e2) ->
    insert_let (g env e1)
      (fun var1 ->
        insert_let (g env e2)
          (fun var2 ->
            FSub (var1, var2), Type.Float
          )
      )
  | Syntax.FMul (e1, e2) ->
    insert_let (g env e1)
      (fun var1 ->
        insert_let (g env e2)
          (fun var2 ->
            FMul (var1, var2), Type.Float
          )
      )
  | Syntax.FDiv (e1, e2) ->
    insert_let (g env e1)
      (fun var1 ->
        insert_let (g env e2)
          (fun var2 ->
            FDiv (var1, var2), Type.Float
          )
      )
  | Syntax.Eq (e1, e2) ->
    (* これはだめなの？
    insert_let (g env e1)
      (fun var1 ->
        insert_let (g env e2)
          (fun var2 ->
            (IfEq (var1, var2, Int 1, Int 0)), Type.Bool
          )
      )
    *)
    g env (Syntax.If (expr, Syntax.Bool true, Syntax.Bool false))
  | Syntax.Neq(e1, e2) ->
    g env (Syntax.Not (Syntax.Eq (e1, e2)))
  | Syntax.Le (e1, e2) ->
    g env (Syntax.If (expr, Syntax.Bool true, Syntax.Bool false))
  | Syntax.Ge (e1, e2) -> D.unimplemented "Ge"
  | Syntax.Lt (e1, e2) -> D.unimplemented "Lt"
  | Syntax.Gt (e1, e2) ->
    g env (Syntax.Not (Syntax.Le (e1, e2)))
  | Syntax.If (e1, e2, e3) ->
    begin [@warning "-4"] match e1 with
    | Syntax.Eq (e1, e2) ->
      insert_let (g env e1)
        (fun var1 ->
            insert_let (g env e2)
            (fun var2 ->
                let e2', t2' = g env e2 in
                let e3', t3' = g env e3 in
                (IfEq (var1, var2, e2', e3')), t2'
            )
        )
    | Syntax.Le (e1, e2) ->
      insert_let (g env e1)
        (fun var1 ->
            insert_let (g env e2)
            (fun var2 ->
                let e2', t2' = g env e2 in
                let e3', t3' = g env e3 in
                (IfLE (var1, var2, e2', e3')), t2'
            )
        )
    (* 解答ではBool falseと比較していたけどなぜ？ *)
    | _ -> g env (Syntax.If (Syntax.Eq (e1, Syntax.Bool true), e2, e3))
    end
  | Syntax.Let (e1, e2, e3) ->
    let e2', _ = g env e2 in
    let e1_t = Type.gentyp() in
    let [@warning "-4"] var_name = match e1 with Syntax.Ident s -> s | _ -> unexpected_type () in
    let updated_env = M.add var_name e1_t env in
    let e3', t3' = g updated_env e3 in
    Let ((var_name, e1_t), e2', e3'), t3'
  | Syntax.LetRec (e1, e2, e3, e4) ->
    let e1_t = Type.gentyp () in
    let [@warning "-4"] var_name = match e1 with Ident s -> s | _ -> unexpected_type () in
    let [@warning "-4"] e2_var_t_pair_list = List.map (
      fun syntax_t ->
        match syntax_t with
        | Syntax.Ident s -> (s, Type.gentyp ())
        | _ -> unexpected_type () ) e2 in
    let updated_env_1 = M.add var_name e1_t env in
    (* e4ではe2に含まれるローカル変数は使わない *)
    let e4', t4' = g updated_env_1 e4 in
    let updated_env_2 = M.add_list e2_var_t_pair_list updated_env_1 in
    let e3', _ = g updated_env_2 e3 in
    LetRec ({name = (var_name, e1_t); args = e2_var_t_pair_list; body = e3'}, e4'), t4'
  | Syntax.App (e1, e2s) ->
    begin [@warning "-4"] match g env e1 with
    | _, Type.Fun (_, t) as normalized_e1 ->
      insert_let normalized_e1 (fun var ->
        let rec bind xs e2s = (* "xs" are identifiers for the arguments *)
            begin match e2s with
            | [] -> App(var, xs), t
            | e2 :: e2s ->
                insert_let (g env e2) (fun e2_var -> bind (xs @ [e2_var]) e2s)
            end
        in
        bind [] e2s) (* left-to-right evaluation *)
    | _ -> unexpected_type ()
    end
