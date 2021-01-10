(* lexer定義の構文
   https://ocaml.org/releases/4.11/htmlman/lexyacc.html#s:ocamllex-syntax
*)

{
    open Parser
}

let eol = ['\n']
let digit = ['0'-'9']

rule token = parse
| eol          { EOL }
| digit+       { INT(int_of_string (Lexing.lexeme lexbuf)) }
| "+"          { PLUS }
| "-"          { MINUS }
| "*"          { TIMES }
| "/"          { DIV }
| "="          { EQ }
| "<="         { LE }
| ">="         { GE }
| "<"          { LT }
| ">"          { GT }
