{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let fisrtCap = ['A'-'Z'] ['A'-'Z''a'-'z''0'-'9']*
let vowelLower = ['a''e''i''o''u']
let vowelCap = ['A''E''I''O''U']
let consonant = ['A'-'Z']#(vowelCap)#(vowelLower)



rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
  | fisrtCap {ATOK}
  | vowelLower* {BTOK}