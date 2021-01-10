open OUnit2

let parse str =
    let lexbuf = Lexing.from_string str in
    Parser.main Lexer.token lexbuf

let test input expected =
    assert_equal (parse input) expected

let suite = "parse string" >::: [

    "[int]" >:: fun _ -> test "1\n" (Int 1)

]

let () = run_test_tt_main suite
