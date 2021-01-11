%{
  open Syntax
%}

%token <int> INT
/* %token <bool> BOOL */
%token <float> FLOAT
/* %token NOT  */
%token PLUS MINUS TIMES DIV
/* %token IF THEN ELSE */
/* %token LET REC IN */
%token EQ LT GT LE GE
/* %token LPAREN RPAREN */
%token EOL
/* %token ARR_CREATE */
/* %token GET */
/* %token PUT */
/* %token SEMICOLON */
/* %token COMMA */

/* %token MINUS_DOT PLUS_DOT AST_DOT SLASH_DOT 謎 */

%left EQ LT GT LE GE
%left PLUS MINUS
%left TIMES DIV

%start main
%type <Syntax.t> main

%%
main:
    | expr EOL
        { $1 }
expr:
    | INT
        { Int($1) }
    | FLOAT
        { Float($1) }
    | expr PLUS expr
        { Add($1, $3) }
    | expr MINUS expr
        { Sub($1, $3) }
    | expr TIMES expr
        { Mul($1, $3) }
    | expr DIV expr
        { Div($1, $3) }
    | expr EQ expr
        { Eq($1, $3) }
    | expr LT expr
        { Lt($1, $3) }
    | expr GT expr
        { Gt($1, $3) }
    | expr LE expr
        { Le($1, $3) }
    | expr GE expr
        { Ge($1, $3) }
