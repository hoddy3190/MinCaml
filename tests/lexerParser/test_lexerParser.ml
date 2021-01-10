open OUnit2
open Printf
open Syntax

let parse str =
    let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf

let print_val t =
    match t with
    | Int i -> sprintf "\"Int %d\"" i
    | _ -> "something but Syntax.t"

let test input expected =
    (* 引数としてtest_ctxtを受けつける関数を返す。ただし今回はtest_ctxtは使わない *)
    fun _ ->
        assert_equal
            expected
            (parse input)
            (* 改行がinputに含まれる可能性があるのでエスケープ *)
            ~msg: (sprintf "input: %S" input)
            ~printer: print_val

let suite = "parse string" >::: [

    "[int]" >:: test
        "1\n"
        (Int 1);

]

let () = run_test_tt_main suite
