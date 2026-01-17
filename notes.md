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