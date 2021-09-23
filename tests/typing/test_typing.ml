open OUnit2
open Printf
open Type

let parse str =
    let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf

let test input (expected:Type.t) =
    (* 引数としてtest_ctxtを受けつける関数を返す。ただし今回はtest_ctxtは使わない *)
    fun _ ->
        assert_equal
            (* 改行がinputに含まれる可能性があるのでエスケープ *)
            ~msg: (sprintf "input: %S" input)
            ~printer: [%derive.show: t]
            expected
            (Typing.g M.empty (parse input))

let suite = "type inference" >::: [

    "Add" >:: test
        "1 + 1\n"
        (Type.Int);

    "Let Rec関数" >:: test
        "let rec fib n = if n <= 1 then n else let a = n - 2 in let b = n - 1 in fib a + fib b in fib 5\n"
        (Type.Var (ref (Some Type.Int)));

]

let () = run_test_tt_main suite
