let rec lang1 l = match l with
    [] -> true
  | x::t -> (x = '1' || x = '0') && lang1 t
;;

let rec other l = match l with
	  [] -> true
	| x::s -> x = '1' && other s
;;

let lang2 l = match l with 
	  [] -> true
	| x::t -> (x = '0' || x = '1') && other t
;;

let rec isLastElement l = match l with
	  [] -> failwith "LISTA VUOTA"
	| x::[] -> x = '0' 
	| x::t -> (x = '1' || x = '0') && isLastElement t
;;

let lang3 l = match l with 
	  [] -> false
	| x::t -> (x = '0') && isLastElement t

  let rec verifyList l = match l with
  [] -> true
| x::t -> (x = '0'|| x = '1') && verifyList t
;;

let filtraLista l = l |> List.filter (fun x -> x = '1')

let rec counter l = match filtraLista l with
  [] -> 0
| _::[] -> 1
| _::t -> 1 + counter t
;;

let lang4 l = counter l = 2 && verifyList l;;

let rec lang5 l = match l with
	  [] -> true
	| '0'::'0'::t -> lang5 t
	| '1'::'1'::t -> lang5 t
	| _::[] -> false
	| _::_::_ -> false
;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
