Ocaml has pattern matching

let foo (x: int) : string = 
    match x with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | _ -> "I'm bored" (* _ matches anything at all *)

Tuple - can combine multiple diff types

let p1 = (10, 20);;
represented as int * int


Synonym
type point2d = int * int;;   You can pass in a variable as a data type
type rgb = int * int * int
type point3d = int * int * int
type student = string * string * int * bool

Shift x coord by given delta

let shift_x (p : point2d) (delta : int) : int * int =       point2d could just be replaced with int * int
    match p with
    | (x, y) -> (x + delta, y)

or 

let shift_x (x, y : point2d) (delta : int) : point2d = 
    (x + delta, y);;

let shift (x, y: point2d) (dx, dy : int * int) : point2d =
    (x + dx, y + dy);;

let shift (pt : point2d) (delta : int * int) : point2d =
    match pt, delta with
    | (x, y), (dx, dy) -> (x + dx, y + dy);;


type my_bool = 
    | True
    | False;;

type color = 
    | Red
    | Blue
    | Yellow
    | Green;;

type rgb = int * int * int;;

let color_to_rgb (c : color) : rgb =
    match c with
    | Red -> (255, 0, 0)
    | Blue -> (0, 0, 255)
    | Green -> (0, 255, 0);;

type distance =
    | Inches of float   Use "of ___" to signify what type it is
    | Feet of float
    | Yards of float;;

type quantity =
    | Fraction of int * int

let string_of_quantity (q : quantity) : string =
    match q with
    | Fraction (n, d) -> string_of_int n ^ "/" ^ string_of_int d


type int_option =
    | NoInt
    | SomeInt of int

let my_div (n : int) (d : int) : int_option =
    if d = 0 then NoInt else SomeInt (n / d);;


type int_list =
    | Empty
    | Cons of int * int_list;;

Recursive Functions

let rec int_list_func (xs : int_list) : ??? =
    match xs with
    | Empty -> ...
    | Cons (x, xs') -> ... x ... int_list_func xs' ...

Compute length of an int list
let rec int_list_length (xs : int_list) : int =
    match xs with
    | Empty -> 0
    | Cons (_, xs') -> 1 + int_list_length xs';;

let rec int_list_sum (xs : int_list) : int =
    match xs with
    | Empty -> 0
    | Cons (x, xs') -> x + int_list_sum xs';;


Binary Tree - either a lead w an int or a branch w 2 binary trees

type int_bintree =
    | Leaf of int
    | Branch of int_bintree * int_bintree;;

let rec sum_int_bintree (bt : int_bintree) : int =
    match bt with
    | Leaf x -> x
    | Branch (bt_l, bt_r) -> sum_int_bintree bt_l + sum_int_bintree bt_r;;

let bt1 =
    Branch (
        Branch (Leaf 1, Leaf 2),
        Branch (
            Branch (Leaf 3, Leaf 4),
            Leaf 5));;


(* Q2 Mutual Recursion *)

let rec alternating (xs : int list) : bool =
  match xs with
  | [] -> true
  | x :: xs' -> if x mod 2 = 0 then g xs' else false
and g (x : int list) : bool =
  match x with
  | [] -> true
  | x :: x' -> if x mod 2 = 0 then false else alternating x'

or (you can use bool params instead of mutual recursion to switch between states)

let rec alternating (which : bool) (xs : int list) : bool =
  if which then
    match xs with
    | [] -> true
    | x :: xs' -> if x mod 2 = 0 then alternating false xs' else false
  else
    match xs with
    | [] -> true
    | x :: xs' -> if x mod 2 = 0 then false else alternating true xs'


Adding stuff to a list

let list1 = [2; 3; 4];;

let list2 = 5 :: list1;;

or

let list2 = [5; 6] :: list1;;


# Specifications

(* Return the larger of two numbers *)
let max (x : int) (y : int) : int = ...

what this gives us is a set:
S = {P | implements "Return the larger of two numbers" }

(* Sort the list *)
let rec sort (xs: int list) : int list = ...

These specs aren't specific/detailed enough

# Formal Specifications

- description of computation (= of a set of possible implementations)
using a formal language - math / logic / programming language

let sort (xs : int list) : int list = ...

- precise (unambiguous) but too general - doesn't restrict the implementaton enough

# Property-based Specifications

- example sort
    (* Sorts the list of ints *)
    let rec sort (xs : int list) : int list = ...

1. Whenever sort xs = ys, ys is sorted
Assume that ys[i] is the element at the ith index in the list

forall indices 0 <= i <= j < length ys. ys[i] <= ys[j]

2. for all elements in ys. elements in xs
for all i. i count i xs = count i ys

forall
count
same_elements

let rec sorted (xs: int list) : bool =
    match xs with
    | [] | [_] -> true
    | x :: y :: xs' -> x <= y && sorted (y :: xs')


let prop_sort_correct (sort: )



Property Based Testing (PBT)
1. Write down properties as predicates (functions that take any value as argument and return true or false depending on whether the property holds)
2. use a random value generator for a (directed) sampling of the value space
3. if counter-example is found, property is invalidated and does not hold
4. if no counter-example is found, we only know that the property holds for the examples tested

QuickCheck in Haskell
Needed:
1. generator for values
2. test runner: check the property on a given number of random values
    - if a counter-example is found it needs to be returned

let gen_list (length_bound : int) (int_bound : int) (_ : unit) : int list =
    let len = Random.int length_bound in
    List.init len (fun _ -> Random.int int_bound)

(* type counter_example = None | Some int list *) = int list option


let rec forall_list (gen : unit -> int list) (prop : int list -> bool) (trials : int) : int list option =
    if trials <= 0 then (* didn't find a counter example *) None else
        let xs = gen () in
        if prop xs then
            forall_list gen prop (trials - 1)
        else
            Some xs

forall_list (gen_list 100 1000) (prop_sort_correct isort) 1000;


Insertion Sort

let rec insert (x : int) (xs : int list) : int list =
    match xs with
    | [] -> [x]
    | y :: xs' -> if x <= y then x :: y :: xs' else y :: insert x xs'

let rec isort (xs : int list) : int list =
    match xs with
    | [] -> []
    | x :: xs' -> insert x (isort xs')


Bintrees

type bintree =
| Leaf of int
| Branch of bintree * bintree

(* Structural recursive template *)
let rec bintree_func (bt : bintree) : ?? =
    match bt with
    | Leaf v -> v
    | Branch (l, r) -> ... bintree_func l ... bintree_func r ...

let rec sum_bintree (bt : bintree) : int =
    match bt with
    | Leaf v -> v
    | Branch (l, r) -> sum_bintree l + sum_bintree r

(*
 /\
/\/\
1234 -> 4321  *)
let rec flip_tree (bt : bintree) : bintree =
    match bt with
    | Leaf v -> Leaf v
    | Branch (l, r) -> Branch(flip_tree r, flip_tree l)

(* Generator for bintrees *) (Same depth for each tree, but you can change with random depths)
let gen_bintree (depth_bound : int) (int_bount : int) (_ : unit) : bintree =
    let rec make_tree (depth : int) : bintree =
        if depth <= 0 then Leaf Random.int (int_bound)
        else Branch (make_tree (depth - 1), make_tree (depth - 1))
    in
    make_tree (Random.int depth_bound)

(* Property checker for bintree *)
let rec forall_bintree (gen : unit) (prop : bintree -> bool) (trials : int) : bintree option =
    if trials <= 0 then None else
        let bt = gen () in
        if prop bt then
            forall_bintree gen prop (trials - 1)
        else
            Some bt

(* Property: flipped tree is not equal to original tree (not true tho ex. leaf 1 or symmetrical tree)*) (false)
let prop_flip_tree_not_eq (flip_tree : bintree -> bintree) : bintree -> bool =
    fun (bt : bintree) ->
        not (flip_tree bt = bt)

(* Property: for all bt, flip_tree (flip_tree bt) = bt *) (true)
let prop_flip_tree_involutive (flip_tree : bintree -> bintree) : bintree -> bool =
    fun (bt : bintree) ->
        flip_tree (flip_tree bt) = bt

let test_flip_tree_not_eq () =
    forall_bintree (gen_bintree 20 100) (prop_flip_tree_not_eq flip_tree) 1000

let test_flip_tree_involutive () =
    forall_bintree (gen_bintree 20 100) (prop_flip_tree_involutive flip_tree) 1000


type entry { tag : string; value : int }

Tagged Bintrees

type tagged_bintree =
| Leaf of entry
| Branch of tagged_bintree * tagged_bintree

let rec sum_tagged_bintree (bt: tagged_bintree) : int =
    match bt with
    (* | Leaf { tag = t; value = v } -> v   Match on whole record *)
    (* | Leaf { tag = _; value = v } -> v   Ignore value of a field *)
    (* | Leaf { value = v; _ } -> v         Ignore any additional fields *)
    | Leaf {value; _} -> value      (* Field name punning *)
    | Branch (l, r) -> sum_tagged_bintree l + sum_tagged_bintree r



let rec collect_tagged (t : string) (bt : tagged_bintree) : int list =
    match bt with
    | Leaf { tag; value } -> if t = tag then [value] else []
    | Branch (l, r) -> collect_tagged t l @ collect_tagged t r


let rec append (xs : int list) (ys : int list) : int list =
    match xs with
    | [] -> ys
    | x :: xs' -> x :: append xs' ys

let rec sum_tagged (t : string) (bt : tagged_bintree) : int =
    match bt with
    | Leaf { tag; value } when t = tag -> value
    | Leaf _ -> 0
    | Branch (l, r) -> sum_tagged t l + sum_tagged t r


Generator and Property Tester for tagged_bintree