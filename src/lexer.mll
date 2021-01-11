(* lexer定義の構文
   https://ocaml.org/releases/4.11/htmlman/lexyacc.html#s:ocamllex-syntax
*)

{
    open Parser
}

let eol = ['\n']
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident = lower (digit | lower | upper | '_')*

rule token = parse
| eol          { EOL }
| space+       { token lexbuf }
| digit+       { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?  { FLOAT(float_of_string (Lexing.lexeme lexbuf))}
| "true"       { BOOL(true) }
| "false"      { BOOL(false) }
| "not"        { NOT }
| "+"          { PLUS }
| "-"          { MINUS }
| "*"          { TIMES }
| "/"          { DIV }
| "+."         { FPLUS }
| "-."         { FMINUS }
| "*."         { FTIMES }
| "/."         { FDIV }
| "="          { EQ }
| "<>"         { NEQ }
| "<="         { LE }
| ">="         { GE }
| "<"          { LT }
| ">"          { GT }
| "if"         { IF }
| "then"       { THEN }
| "else"       { ELSE }
| "let"        { LET }
| "in"         { IN }
(* trueやifにもマッチするので他のregexよりも後に書く *)
| ident        { IDENT(Lexing.lexeme lexbuf) }
