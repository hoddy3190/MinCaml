%{
  open Syntax
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NOT
%token PLUS MINUS TIMES DIV
%token FPLUS FMINUS FTIMES FDIV
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

%left EQ LT GT LE GE
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIV FTIMES FDIV

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
    | BOOL
        { Bool($1) }
    | NOT expr
        { Not($2) }
    | expr PLUS expr
        { Add($1, $3) }
    | expr MINUS expr
        { Sub($1, $3) }
    | expr TIMES expr
        { Mul($1, $3) }
    | expr DIV expr
        { Div($1, $3) }
    | expr FPLUS expr
        { FAdd($1, $3) }
    | expr FMINUS expr
        { FSub($1, $3) }
    | expr FTIMES expr
        { FMul($1, $3) }
    | expr FDIV expr
        { FDiv($1, $3) }
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
