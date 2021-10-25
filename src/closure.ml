type closure = { entry : Id.l; actual_fv : Id.t list }
type t = (* クロージャ変換後の式。本家からコピー *)
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
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  (* | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l *)
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t }
type prog = Prog of fundef list * t


let rec fv e =
  match e with
  | Unit -> S.empty
  | Int _ -> S.empty
  | Float _ -> S.empty
  | Neg s -> S.singleton s
  | Add (s1, s2) -> S.of_list [s1; s2]
  | Sub (s1, s2) -> S.of_list [s1; s2]
  | FNeg s -> S.singleton s
  | FAdd (s1, s2) -> S.of_list [s1; s2]
  | FSub (s1, s2) -> S.of_list [s1; s2]
  | FMul (s1, s2) -> S.of_list [s1; s2]
  | FDiv (s1, s2) -> S.of_list [s1; s2]
  | IfEq (s1, s2, e1, e2) -> S.union (S.union (S.of_list [s1; s2;]) (fv e1)) (fv e2)
  | IfLE (s1, s2, e1, e2) -> S.union (S.union (S.of_list [s1; s2;]) (fv e1)) (fv e2)
  | Let ((s1, t1), e1, e2) -> S.union (fv e1) (S.remove s1 (fv e2))
  | Var s -> S.singleton s
  | MakeCls ((x, t), { entry = l_s; actual_fv = s_list }, e) ->
    S.remove x (S.union (S.of_list s_list) (fv e))
  | AppCls (s, s_list) -> S.of_list (s :: s_list)
  | AppDir (_, s_list) -> S.of_list s_list

let toplevel : fundef list ref = ref []

(* env: 型環境
   known: 自由変数がないとわかっていて、普通に呼び出せる関数の集合 *)
let rec g env known e =
  match e with
  | KNormal.Unit -> Unit
  | KNormal.Int i -> Int i
  | KNormal.Float f -> Float f
  | KNormal.Neg s -> Neg s
  | KNormal.Add (s1, s2) -> Add (s1, s2)
  | KNormal.Sub (s1, s2) -> Sub (s1, s2)
  | KNormal.FNeg s -> FNeg s
  | KNormal.FAdd (s1, s2) -> FAdd (s1, s2)
  | KNormal.FSub (s1, s2) -> FSub (s1, s2)
  | KNormal.FMul (s1, s2) -> FMul (s1, s2)
  | KNormal.FDiv (s1, s2) -> FDiv (s1, s2)
  | KNormal.IfEq (s1, s2, e1, e2) -> IfEq (s1, s2, g env known e1, g env known e2)
  | KNormal.IfLE (s1, s2, e1, e2) -> IfLE (s1, s2, g env known e1, g env known e2)
  | KNormal.Let ((s1, t1), e1, e2) -> D.unimplemented "Let"
  | KNormal.Var s -> Var s
  | KNormal.LetRec ({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) ->
    (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
       xに自由変数がない(closureを介さずdirectに呼び出せる)
       と仮定し、knownに追加してe1をクロージャ変換してみる *)
    let toplevel_backup = !toplevel in (* なにこれ？ *)
    let env' = M.add x t env in
    let known' = S.add x known in
    let e1' = g (M.add_list yts env') known' e1 in
    (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
    (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
    (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) *)
    let e1'_fv = List.fold_left
      (fun acc (s, _) ->
        S.remove s acc
      )
      (fv e1')
      yts
    in
    let known', e1' =
      if S.is_empty e1'_fv then
        known', e1'
      else
        (
          toplevel := toplevel_backup;
          let e1' = g (M.add_list yts env') known e1 in
          known, e1'
        )
    in
    let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in
    let zts = List.map (fun z -> z, M.find z env') zs in
    toplevel := { name = (Id.L x, t); args = yts; formal_fv = zts; body = e1' } :: !toplevel;
    let e2' = g env' known' e2 in
    if S.mem x (fv e2') then
      MakeCls ((x, t), { entry = Id.L x; actual_fv = zs }, e2')
    else
      e2'
  | KNormal.App (s, s_list) -> D.unimplemented "App"
