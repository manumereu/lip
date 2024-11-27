{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "0" {ZERO}
  | "and" {AND}
  | "or" { OR }
  | "not" { NOT }
  | "iszero" {ISZERO}
  | "pred" {PRED}
  | "succ" {SUCC}
  | eof { EOF }
