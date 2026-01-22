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