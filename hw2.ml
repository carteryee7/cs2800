(* If you would like, please write here how many hours you spent on the assignment: 
  XXXX
*)

(* Q1 *)
let rec concat (xs : string list) : string =
  match xs with
  | [] -> ""
  | x :: xs' -> x ^ concat xs';;

(* Q2 Mutual Recursion *)
let rec alternating (xs : int list) : bool =
  match xs with
  | [] -> true
  | x :: xs' -> if x mod 2 = 0 then g xs' else false
and g (x : int list) : bool =
  match x with
  | [] -> true
  | x :: x' -> if x mod 2 = 0 then false else alternating x'

(* Q3 *)

type int_tree =
  | Leaf
  | Node of (int_tree * int * int_tree)

let rec gen_int_tree (depth : int) (bound : int) : int_tree =
  if depth <= 0 then
    Leaf
  else
    Node (gen_int_tree (depth - 1) bound, Random.int bound, gen_int_tree (depth - 1) bound)

let rec string_of_int_tree (t : int_tree) : string =
  match t with
  | Leaf -> "leaf"
  | Node (x, y, z) -> string_of_int_tree x ^ string_of_int y ^ string_of_int_tree z

(* Q4 *)

let rec map_int_tree (f : int -> int) (t : int_tree) : int_tree =
  match t with
  | Leaf -> Leaf
  | Node (x, y, z) -> Node (map_int_tree f x, f y, map_int_tree f z)

(* Q5 *)
let rec int_tree_max (t : int_tree) : int option = failwith "TODO"
  

(* Q6 *)

let rec sorted (t : int_tree) : bool = failwith "TODO"
