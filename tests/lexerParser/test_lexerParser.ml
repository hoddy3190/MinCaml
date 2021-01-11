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

    "[float]1.23" >:: test
        "1.23\n"
        (Float 1.23);

    "[float]12." >:: test
        "12.\n"
        (Float 12.);

    "[float]12e03" >:: test
        "12e03\n"
        (Float 12e03);

    "[float]12.3E-45" >:: test
        "12.3E-45\n"
        (Float 12.3E-45);

    "[bool]true" >:: test
        "true\n"
        (Bool true);

    "[not]" >:: test
        "not 1 = 1\n"
        (Not (
            Eq (
                Int 1, Int 1)));

    "加減乗除の優先度は乗除>加減" >:: test
        "1 - 3 - 6 / 3 * 4 + 2 * 3\n"
        (Add (
            Sub (
                Sub (
                    Int 1, Int 3),
                Mul (
                    Div (
                        Int 6, Int 3),
                    Int 4)),
            Mul (
                Int 2, Int 3)));

    "浮動小数点加減乗除" >:: test
        "1.0 +. 0.0 -. 4.1 /. 5.3 *. 6.7\n"
        (FSub (
            FAdd (
                Float 1.0, Float 0.0),
            FMul (
                FDiv (
                    Float 4.1, Float 5.3),
                Float 6.7)));

    "等式" >:: test
        "1 + 55 = 8 * 7\n"
        (Eq (
            Add (
                Int 1, Int 55),
            Mul (
                Int 8, Int 7)));

    "等式の否定" >:: test
        "1 + 55 <> 8 * 7\n"
        (Neq (
            Add (
                Int 1, Int 55),
            Mul (
                Int 8, Int 7)));

    "不等式" >:: test
        "1 + 2 <= 9 / 2\n"
        (Le (
            Add (
                Int 1, Int 2),
            Div (
                Int 9, Int 2)));

]

let () = run_test_tt_main suite
