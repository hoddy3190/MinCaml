%{
  open Syntax
%}

%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NOT
%token PLUS MINUS TIMES DIV
%token FPLUS FMINUS FTIMES FDIV
%token IF THEN ELSE
%token LET REC IN
%token EQ NEQ LT GT LE GE
/* %token LPAREN RPAREN */
%token EOL
/* %token ARR_CREATE */
/* %token GET */
/* %token PUT */
/* %token SEMICOLON */
/* %token COMMA */

%left EQ NEQ LT GT LE GE
%left PLUS MINUS FPLUS FMINUS
%left TIMES DIV FTIMES FDIV

%start main
%type <Syntax.t> main

%%
main:
    | expr EOL
        { $1 }

var:
    | IDENT
        { Ident($1) }

params:
    | var params
        { $1 :: $2 }
    | var
        { [$1] }

expr:
    | var
        { $1 }
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
    | expr NEQ expr
        { Neq($1, $3) }
    | expr LT expr
        { Lt($1, $3) }
    | expr GT expr
        { Gt($1, $3) }
    | expr LE expr
        { Le($1, $3) }
    | expr GE expr
        { Ge($1, $3) }
    | IF expr THEN expr ELSE expr
        { If($2, $4, $6) }
    | LET var EQ expr IN expr
        { Let($2, $4, $6) }
    | LET REC var params EQ expr IN expr
        { LetRec($3, $4, $6, $8) }
