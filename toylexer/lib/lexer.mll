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
let vowel = vowelCap | vowelLower
let consonant = ['a'-'z' 'A'-'Z'] - vowel
let digit = ['0'-'9']
let integer = ['-']? digit+
let decimal = ['-']? digit* '.' digit+
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let hex_prefix = "0x" | "0X"

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
  | consonant* vowel? consonant* {CTOK}
  | integer | decimal {DTOK}
  | hex_prefix hex_digit+ {ETOK}