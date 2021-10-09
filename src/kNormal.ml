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
    match k_normal_e with
    | Var _ -> D.unimplemented "Var"
    | Add (s1, s2) ->
      let new_var_name = Id.gentmp t in
      let e', t' = k new_var_name in
      Let ((new_var_name, t), k_normal_e, e'), t'
    | _ -> D.unimplemented "Other"

(* 変数の型環境envとK正規化前の式とを受け取り、K正規化後の式とその型とを組にして返す *)
let rec g env expr =
  match expr with
  | Syntax.Ident s -> D.unimplemented "Ident"
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
  | Syntax.Sub (e1, e2) -> D.unimplemented "Sub"
  | Syntax.Mul (e1, e2) -> D.unimplemented "Mul"
  | Syntax.Div (e1, e2) -> D.unimplemented "Div"
  | Syntax.FAdd (e1, e2) -> D.unimplemented "FAdd"
  | Syntax.FSub (e1, e2) -> D.unimplemented "FSub"
  | Syntax.FMul (e1, e2) -> D.unimplemented "FMul"
  | Syntax.FDiv (e1, e2) -> D.unimplemented "FDiv"
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
    begin match e1 with
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
  | Syntax.Let (e1, e2, e3) -> D.unimplemented "Let"
  | Syntax.LetRec (e1, e2, e3, e4) -> D.unimplemented "LetRec"
  | Syntax.App (e1, e2) -> D.unimplemented "App"
