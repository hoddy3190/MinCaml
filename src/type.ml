type t = (* MinCamlの型を表現するデータ型 *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  (*  型変数
      型推論で使用
      Var(ref None)     : 未定義の型変数
                          https://github.com/esumii/min-caml/blob/a4f74c2157de178813f712458f782688dc60e5e3/tutorial-mincaml-8.htm#L469-L473
      Var(ref (Some t)) : 型tだと推論された型変数
  *)
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)
