open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)

let occorrenze a l = let m = l |> List.filter(fun x -> x=a) |> List.length in m + 1;;

let rimuovi a l = l |> List.filter(fun x -> x<>a);;

let frequency n l = 
  let rec listaNonOrdinata n t = if n>0 then 
  match t with
      [] -> []
     | x::t -> [(x, occorrenze x t)] @ listaNonOrdinata (n-1) (rimuovi x t)
    else 
      [] in List.sort(fun(_,a)(_,b) -> if(a<b) then 1 else -1) (listaNonOrdinata n l);;