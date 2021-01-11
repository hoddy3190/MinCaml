open OUnit2
open Printf
open Syntax

let parse str =
    let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf

let test input expected =
    (* 引数としてtest_ctxtを受けつける関数を返す。ただし今回はtest_ctxtは使わない *)
    fun _ ->
        assert_equal
            (* 改行がinputに含まれる可能性があるのでエスケープ *)
            ~msg: (sprintf "input: %S" input)
            ~printer: [%derive.show: t]
            expected
            (parse input)

let suite = "parse string" >::: [

    "[int]" >:: test
        "1\n"
        (Int 1);

]

let () = run_test_tt_main suite
