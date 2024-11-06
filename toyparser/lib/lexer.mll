{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let exum = "0x" ['0'-'9''A'-'F''a'-'f']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { FRAC }
  | num { CONST (Lexing.lexeme lexbuf) }
  | exum { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
