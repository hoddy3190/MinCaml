open Syntax
open Err

exception OccurCheckError
let occur_check_error () = raise OccurCheckError

exception NotEqualType
let not_equal_type () = raise NotEqualType

(* 未定義の型変数が入っていないことをチェックする *)
let rec occur t =
  match t with
  | Type.Unit -> D.unimplemented "Unit"
  | Type.Bool -> false
  | Type.Int -> false
  | Type.Float -> false
  | Type.Fun (t_list, t) (* arguments are uncurried *) -> D.unimplemented "Fun"
  | Type.Tuple t_list -> D.unimplemented "Tuple"
  | Type.Array t -> D.unimplemented "Array"
  | Type.Var {contents = Some t'} -> occur t'
  | Type.Var {contents = None} -> true

(*  次のロジックにしたがって、型変数Var Noneへの代入をする
    validationも兼ねているので、t1とt2が両方型変数でなく、かつt1とt2の型が一致しない場合は例外をはく
    pattern 1. Var (Some t), Var (Some t)  -> 物理的等価の場合はなにもしない。そうでなければそれぞれSomeのパラメをたどっていき、いきついた型t1'とt2'をunifyする
    pattern 2. Var (Some t), Var None      -> Var (Some t)でなくなるまでtをたどっていき、いきついた型をt'とVar Noneをunifyする
    pattern 3. Var (Some t), 型変数でない型   -> Var (Some t)でなくなるまでtをたどっていき、いきついた型をt'と型変数でない型をunifyする
    pattern 4. Var None, Var (Some t)      -> Var (Some t)でなくなるまでtをたどっていき、Var Noneといきついた型をt'をunifyする
    pattern 5. Var None, Var None          -> なにもしない
    pattern 6. Var None, 型変数でない型       -> occur_checkの後、型変数でない型を型変数に代入する
    pattern 7. 型変数でない型, Var (Some t)   -> Var (Some t)でなくなるまでtをたどっていき、型変数でない型といきついた型をt'をunifyする
    pattern 8. 型変数でない型, Var None       -> occur_checkの後、型変数でない型を型変数に代入する
    pattern 9. 型変数でない型, 型変数でない型    -> 期待されている型と、実際の型が一致する場合はなにもしない。一致しない場合は例外
*)
let [@warning "-4"] rec unify t1 t2 =
  match t1, t2 with
  | Type.Var ({contents = Some t1'} as r1), Type.Var ({contents = Some _} as r2) -> (* pattern 1 *)
    if r1 == r2 then () else unify t1' t2
  | Type.Var {contents = Some t1'}, Type.Var {contents = None} ->                   (* pattern 2 *)
    unify t1' t2
  | Type.Var {contents = Some t1'}, _ ->                                            (* pattern 3 *)
    unify t1' t2
  | Type.Var {contents = None}, Type.Var {contents = Some t2'} ->                   (* pattern 4 *)
    unify t1 t2'
  | Type.Var {contents = None}, Type.Var {contents = None} ->                       (* pattern 5 *)
    ()
  | Type.Var ({contents = None} as r1), _ ->                                        (* pattern 6 *)
    if occur t2 then occur_check_error () else r1 := Some t2
  | _, Type.Var {contents = Some t2'} ->                                            (* pattern 7 *)
    unify t1 t2'
  | _, Type.Var ({contents = None} as r2) ->                                        (* pattern 8 *)
    if occur t1 then occur_check_error () else r2 := Some t1
  | _ , _ ->                                                                        (* pattern 9 *)
    if t1 == t2 then () else not_equal_type ()


(* 式exprの型を推論して返す *)
(* env: 型環境（変数の名前から、その型への写像） *)
let rec g env (expr:t) =
  match expr with
  | Ident string -> D.unimplemented "Ident"
  | Int _ -> Type.Int
  | Float _ -> Type.Float
  | Bool _ -> Type.Bool
  | Not e -> unify Type.Bool (g env e); Type.Bool
  | Neg t -> D.unimplemented "Neg"
  | Add (t, t2) -> D.unimplemented "Add"
  | Sub (t, t2) -> D.unimplemented "Sub"
  | Mul (t, t2) -> D.unimplemented "Mul"
  | Div (t, t2) -> D.unimplemented "Div"
  | FAdd (t, t2) -> D.unimplemented "FAdd"
  | FSub (t, t2) -> D.unimplemented "FSub"
  | FMul (t, t2) -> D.unimplemented "FMul"
  | FDiv (t, t2) -> D.unimplemented "FDiv"
  | Eq (t, t2) -> D.unimplemented "Eq"
  | Neq (t, t2) -> D.unimplemented "Neq"
  | Le (t, t2) -> D.unimplemented "Le"
  | Ge (t, t2) -> D.unimplemented "Ge"
  | Lt (t, t2) -> D.unimplemented "Lt"
  | Gt (t, t2) -> D.unimplemented "Gt"
  | If (t, t2, t3) -> D.unimplemented "If"
  | Let (e1, e2, e3) ->
    let e1_t = Type.gentyp() in
    unify e1_t (g env e2);
    let [@warning "-4"] var_name = match e1 with Ident s -> s | _ -> unexpected_type () in
    let updated_env = M.add var_name e1_t env in
    g updated_env e3;
  | LetRec (t, t2, t3, t4) -> D.unimplemented "LetRec"
