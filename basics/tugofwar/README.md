# Tug of war

Create a new project with the following command:
```
dune init project tugofwar
```

Then, copy the files [bin/main.ml](bin/main.ml) and [lib/tugofwar.ml](lib/tugofwar.ml)
in the folders `tugofwar/bin` and `tugofwar/lib`, respectively.

The main routine in [bin/main.ml](bin/main.ml) reads a line from the stdin.
The expected format is a sequence of symbols A, B, and =, like e.g.:
```
AAAAA===BB
```
This string represent a tug of war game.
The leftmost part represents the players of the team A,
the = represent the rope,
and the rightmost B represents the players of the team B.
The winner of a game is the team with the most players.

As you see from the main routine:
```ocaml
let () = match read_line () with
    Some s -> let l = toklist_of_string s in
    if valid l then print_endline (string_of_winner (win l))
    else print_endline "bad input"
  | None -> print_endline "no winner"
```
you must implement the four missing functions:
- `toklist_of_string` transforms the input string into a list of tokens A, B, X
- `valid` determines is a list of tokens is valid, i.e. it belongs to the language A*=*B*
- `win` determines the winner of a game (token `A` if the winner is team A, token `B` if the winner is team B, and `X` for a tie)
- `string_of_winner` transforms a token into a string.

Check the file [lib/tugofwar.ml] for the signature of these functions,  
complete their implementation, and then test the project using `dune exec tugofwar`.

Recall that you can use the command:
```
dune utop lib
```
from the `tugofwar` directory to test the functions.
To use it, first open the Tugofwar library with:
```
open Tugofwar;;
```
or write the line in the file `tugofwar/.ocamlinit`.