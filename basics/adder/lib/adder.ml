(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let addlist = List.fold_left(+) 0;;