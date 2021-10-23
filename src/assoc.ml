open KNormal


(* let x1 = (let x2 = (let x3 = e1 in e2) in e3) in e4 という式を受け取ったら
   let x3 = e1 in let x2 = e2 in let x1 = e3 in e4 という形で返す
   ただしe1, e2, e3, e4はネストしていないものとする
   Let x3, e1, (Let x2, e2, (Let x1, e3, e4))
*)

let rec f e =
  match e with
  | Unit -> D.unimplemented "Unit"
  | Int i -> D.unimplemented "Int"
  | Float f -> D.unimplemented "Float"
  | Neg s -> D.unimplemented "Neg"
  | Add (s1, s2) -> D.unimplemented "Add"
  | Sub (s1, s2) -> D.unimplemented "Sub"
  | FNeg s -> D.unimplemented "FNeg"
  | FAdd (s1, s2) -> D.unimplemented "FAdd"
  | FSub (s1, s2) -> D.unimplemented "FSub"
  | FMul (s1, s2) -> D.unimplemented "FMul"
  | FDiv (s1, s2) -> D.unimplemented "FDiv"
  | IfEq (s1, s2, e1, e2) -> IfEq (s1, s2, f e1, f e2)
  | IfLE (s1, s2, e1, e2) -> IfLE (s1, s2, f e1, f e2)
  | Let ((s1, t1), e1, e2) ->
    (* たとえば、Let x1, (Let x2, (Let x3, e1, e2), e3), e4のとき（ただしe1, e2, e3, e4はLet式でないものとする）
       Let x3, e1, (Let x2, e2, (Let x1, e3, e4)) と返す

       まず(Let x2, (Let x3, e1, e2), e3)を簡略化すると
       Let x3, e1, (Let x2, e2, e3)となる

       (Let x2, e2, e3)を(Let x2, e2, (Let x1, e3, e4))の形にしないといけない

       Letの第3項目がLet式かそうでないかで処理を変える

       Let a, b, (Let c, d, (Let e, f, ...)) と再帰的に定義する *)


    (* e1, e2についてもfをしないといけない *)
    (* e1', e2'はLetのネストがない状態になっている *)
    let e1' = f e1 in
    let e2' = f e2 in
    (* あとは、e1'がLetの形になっていてネストになっているのを考慮すればよい
       e.g. let (s1:t1) = (let ... in let ... in ...) in e2'
    *)
    let rec insert = function [@warning "-4"]
    (* ↑の例のように、e1'_e2がまだLet文である可能性がある。e1'_e1がLet文であることはない。 *)
    | Let (e1'_st, e1'_e1, e1'_e2) -> Let (e1'_st, e1'_e1, insert e1'_e2)
    | LetRec (fundef, e1'_e2) -> LetRec(fundef, insert e1'_e2)
    | e -> Let ((s1, t1), e, e2')
    in
    insert e1'
  | Var s -> D.unimplemented "Var"
  | LetRec (fundef, e) -> D.unimplemented "LetRec"
  | App (s, s_list) -> D.unimplemented "App"