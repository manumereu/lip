%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token EOF
%token ZERO
%token ISZERO
%token PRED
%token SUCC
%token OR
%token AND
%token NOT 

%nonassoc ELSE
%left OR
%left AND 
%left NOT 
%left ISZERO
%left PRED SUCC 

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  | ZERO {Zero}
  | SUCC; e=expr { Succ e} 
  | PRED; e=expr { Pred e}
  | ISZERO; e=expr { IsZero e }
  | NOT; e1=expr {Not(e1)}
  | e1=expr; OR; e2=expr {Or(e1,e2)}
  | e1=expr; AND; e2=expr {And(e1,e2)}
;