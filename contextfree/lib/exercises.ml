open Types

(* Use this grammar structure as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S0";
        S --> "1S1";
        S --> "";
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar = {
  symbols = [ S ];
  terminals = [ '0'; '1' ];
  productions =
    [
      S --> "0S1";   (* Aggiunge un 0 all'inizio e un 1 alla fine *)
      S --> "";      (* Stringa vuota, caso base *)
    ];
  start = S;
}

(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar = {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S0";  (* Enclose the string with two 0s *)
        S --> "1S1";  (* Enclose the string with two 1s *)
        S --> "0";    (* Single character palindrome: "0" *)
        S --> "1";    (* Single character palindrome: "1" *)
        S --> "";     (* Empty string, which is trivially a palindrome *)
      ];
    start = S;
  }



(* #### Exercise 3, medium (balanced_parentheses)*)
let balanced_parentheses : grammar =
  {
    symbols = [ S ];
    terminals = [ '('; ')'; '['; ']'; '{'; '}' ];
    productions =
      [
        S --> "(S)";   (* Matching parentheses: () *)
        S --> "[S]";   (* Matching brackets: [] *)
        S --> "{S}";   (* Matching braces: {} *)
        S --> "SS";    (* Concatenation of two valid balanced sequences *)
        S --> "";      (* Empty string, which is balanced by definition *)
      ];
    start = S;
  }



(* #### Exercise 4, hard (same_amount)

   Hint 1: you can use 'a' and 'b' for terminals.
   Hint 2: think of the language of words where the number of 0s is one greater
   than the number of 1s and viceversa, then combine them.
*)
let same_amount : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S1";   (* Add a 0 and a corresponding 1 *)
        S --> "1S0";   (* Add a 1 and a corresponding 0 *)
        S --> "SS";    (* Concatenate two balanced sequences *)
        S --> "";      (* Empty string, which trivially has equal 0s and 1s *)
      ];
    start = S;
  }
