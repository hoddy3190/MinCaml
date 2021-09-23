open Syntax
open Err

(* 未定義の型変数が入っていないことをチェックする *)
let rec occur r t =
  match t with
  | Type.Unit -> D.unimplemented "Unit"
  | Type.Bool -> false
  | Type.Int -> false
  | Type.Float -> false
  | Type.Fun (t_list, t) (* arguments are uncurried *) ->
    List.exists (fun t -> occur r t) (t :: t_list)
  | Type.Tuple t_list -> D.unimplemented "Tuple"
  | Type.Array t -> D.unimplemented "Array"
  | Type.Var r1 when r == r1 -> true (* 同じ参照（指す型変数が同じ）だったらtrueにする *)
  | Type.Var {contents = Some t'} -> occur r t'
  | Type.Var {contents = None} -> false (* 同じ{contents = None}でも違う参照ならfalseにする *)

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
    pattern 9. 型変数でない型, 型変数でない型    -> パラメつきでない型の場合、期待されている型と、実際の型が一致する場合はなにもしない。一致しない場合は例外
                                              パラメつきの型の場合、それぞれのパラメからひとつずつとりだしunifyする
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
    if occur r1 t2 then occur_check_error t1 t2 else r1 := Some t2
  | _, Type.Var {contents = Some t2'} ->                                            (* pattern 7 *)
    unify t1 t2'
  | _, Type.Var ({contents = None} as r2) ->                                        (* pattern 8 *)
    if occur r2 t1 then occur_check_error t1 t2 else r2 := Some t1
  | Type.Unit, Type.Unit -> D.unimplemented "unify Unit"
  | Type.Bool, Type.Bool -> ()
  | Type.Int, Type.Int -> ()
  | Type.Float, Type.Float -> ()
  | Type.Fun (t_list1, t1), Type.Fun (t_list2, t2) ->
    (* t_list1とt_list2の長さが違っていたら例外にしたいが、
       iter2は'a listと'b listの長さが違うとエラーを出すのでそれですませる *)
    List.iter2 (fun t1 t2 -> unify t1 t2) (t_list1 @ [t1]) (t_list2 @ [t2])
  | Type.Tuple _, Type.Tuple _ -> D.unimplemented "unify Tuple"
  | Type.Array _, Type.Array _ -> D.unimplemented "unify Array"
  | _ , _ ->                                                                        (* pattern 9 *)
    if t1 == t2 then () else not_equal_type t1 t2


(* 式exprの型を推論して返す *)
(* env: 型環境（変数の名前から、その型への写像） *)
let rec g env (expr:t) =
  match expr with
  | Ident s -> M.find s env
  | Int _ -> Type.Int
  | Float _ -> Type.Float
  | Bool _ -> Type.Bool
  | Not e -> unify Type.Bool (g env e); Type.Bool
  | Neg e -> unify Type.Int (g env e); Type.Int
  | Add (e1, e2) ->
    unify Type.Int (g env e1);
    unify Type.Int (g env e2);
    Type.Int
  | Sub (e1, e2) ->
    unify Type.Int (g env e1);
    unify Type.Int (g env e2);
    Type.Int
  | Mul (e1, e2) ->
    unify Type.Int (g env e1);
    unify Type.Int (g env e2);
    Type.Int
  | Div (e1, e2) ->
    unify Type.Int (g env e1);
    unify Type.Int (g env e2);
    Type.Int
  | FAdd (e1, e2) ->
    unify Type.Float (g env e1);
    unify Type.Float (g env e2);
    Type.Float
  | FSub (e1, e2) ->
    unify Type.Float (g env e1);
    unify Type.Float (g env e2);
    Type.Float
  | FMul (e1, e2) ->
    unify Type.Float (g env e1);
    unify Type.Float (g env e2);
    Type.Float
  | FDiv (e1, e2) ->
    unify Type.Float (g env e1);
    unify Type.Float (g env e2);
    Type.Float
  | Eq (e1, e2) ->
    unify (g env e1) (g env e2);
    Type.Bool
  | Neq(e1, e2) ->
    unify (g env e1) (g env e2);
    Type.Bool
  | Le (e1, e2) ->
    unify (g env e1) (g env e2);
    Type.Bool
  | Ge (e1, e2) ->
    unify (g env e1) (g env e2);
    Type.Bool
  | Lt (e1, e2) ->
    unify (g env e1) (g env e2);
    Type.Bool
  | Gt (e1, e2) ->
    unify (g env e1) (g env e2);
    Type.Bool
  | If (e1, e2, e3) ->
    let inferred_e1_t = g env e1 in
    unify Type.Bool inferred_e1_t;
    let inferred_e2_t = g env e2 in
    unify (g env e2) (g env e3);
    inferred_e2_t (* e3でもよい *)
  | Let (e1, e2, e3) ->
    (*
      1. envにe1を追加 - お手本では3のあとに1をやっているが、LetRecと順番を揃えたほうがわかりやすいかなと思った
      2. e2を型推論
      3. e1の型変数に1の結果をunify
      4. e3を型推論
    *)
    let e1_t = Type.gentyp() in
    let [@warning "-4"] var_name = match e1 with Ident s -> s | _ -> unexpected_type () in
    let updated_env = M.add var_name e1_t env in
    unify e1_t (g updated_env e2);
    g updated_env e3;
  | LetRec (e1, e2, e3, e4) ->
    (*
      1. envにe1, e2を仮で追加 - e3にはe1,e2が含まれているので2に入る前にenvを更新する必要がある
      2. e3を型推論 - envに追加したe1, e2が更新されるかもしれない
      3. e1の型変数に3の結果をunify
      4. e4を型推論
    *)
    let e1_t = Type.gentyp () in
    let [@warning "-4"] var_name = match e1 with Ident s -> s | _ -> unexpected_type () in
    let [@warning "-4"] e2_var_t_pair_list = List.map (
      fun syntax_t ->
        match syntax_t with
        | Ident s -> (s, Type.gentyp ())
        | _ -> unexpected_type () ) e2 in

    let updated_env = M.add_list ((var_name, e1_t) :: e2_var_t_pair_list) env in
    (*
      e3にはe1やe2を含んだ式である。
      e3を型推論する過程で、e1やe2も型推論される。
      e1やe2はenvにはVar Noneの参照が登録されている。型推論の過程でunifyされるとVar Noneの参照先が別のなにかにすげ替わりうる。
      envに登録してある型も同じところを参照しているため、すげ替わったらこちらも更新される
    *)
    let inferred_e3_t = g updated_env e3 in
    unify e1_t inferred_e3_t;
    g updated_env e4
  | App (e1, e2) ->
    let t = Type.gentyp () in (* e1の返り値の型 *)
    unify (Type.Fun (List.map (g env) e2, t)) (g env e1);
    t
