(* If you would like, please write here how many hours you spent on the assignment: 
  2
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
  | Node (l, i, r) -> string_of_int_tree l ^ string_of_int i ^ string_of_int_tree r

(* Q4 *)

let rec map_int_tree (f : int -> int) (t : int_tree) : int_tree =
  match t with
  | Leaf -> Leaf
  | Node (l, i, r) -> Node (map_int_tree f l, f i, map_int_tree f r)

(* Q5 *)
let rec int_tree_max (t : int_tree) : int option =
  match t with
  | Leaf -> None
  | Node (l, i, r) ->
    let left_max = int_tree_max l in
    let right_max = int_tree_max r in
      match left_max, right_max with
      | None, None -> Some i
      | None, Some x | Some x, None -> Some (max x i)
      | Some x, Some y -> Some (max y (max x i))

(* Q6 *)

let rec sorted (t : int_tree) : bool =
  match t with
  | Leaf -> true
  | Node (x, i, y) ->
    match int_tree_max x, int_tree_max y with
    | None, None -> if i = 0 then true else false
    | None, Some x -> if i <= x then true else false
    | Some x, None -> if i >= x then true else false
    | Some x, Some y -> if (i >= x) && (i <= y) then true else false
