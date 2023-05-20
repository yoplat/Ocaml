(* Let in expression *)
let x = 50 in
x * x

let rec range a b = if a >= b then [] else a :: range (a + 1) b
let rec factorial = function 0 | 1 -> 1 | n -> n * factorial (n - 1)

let rev l =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x :: acc) xs in
  aux [] l

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

(* Constructing a polymorphic data type *)
let tree1 = Node (Leaf, 5, Leaf)

(* Defining expeceptions types *)
exception E2 of string

(* Raising and handling exceptions *)
(* Prints the exception if not handled *)
let div a b = if b = 0 then raise (E2 "division by zero") else a / b
let safe_div a b = try div a b with E2 _ -> 0

(* Using option type as opposed to exceptions *)
let opt_div a b = if b = 0 then None else Some (a / b)
let unpack = function None -> 0 | Some x -> x

(* Imperative ocaml *)
let r = ref 0;;

(* Reassigning value *)
r := 10;;

(* Unpacking the value *)
!r

(* For loops *)
let print_n n =
  for row = 1 to n do
    print_int row;
    print_char ' '
  done;
  let t = ref 1 in
  while !t < 5 do
    print_string "d";
    t := !t + 2
  done

(* Imperative ocaml requires ;; after declaration *)
let arr = [| 1; 2; 3 |];;

(* Accessing elements *)
arr.(0) <- 1

(* Record types *)
type person = { first_name : string; surname : string; mutable age : int };;

(* Printing utility *)
let s = "ciao" in
Printf.printf "%s has %i characters\n" s (String.length s)

let rec list_max = function
  | [] -> failwith "list_max called on empty list"
  | [ x ] -> x (* Single element list *)
  | h :: tl -> max h (list_max tl)

let list_max_tail ls =
  if ls = [] then failwith "list_max called on empty list"
  else
    let rec aux acc = function
      | [] -> acc
      | x :: xs -> if x > acc then aux x xs else aux acc xs
    in
    aux (List.hd ls) (List.tl ls)

(* Goes in stack overflow with large lists *)
let rec range a b = if a > b then [] else a :: range (a + 1) b

(* Doesn't go into stack overflow even with large lists *)
let range_tail a b =
  let rec aux acc a b =
    if a > b then List.rev acc else aux (a :: acc) (a + 1) b
  in
  aux [] a b

(* Doesnt need to be reversed *)
let range_tail_fast a b =
  let rec aux acc a b = if b < a then acc else aux (b :: acc) a (b - 1) in
  aux [] a b

let rec sort lst = match lst with [] -> [] | h :: tl -> insert h (sort tl)

(* and keyword to create another function used by sort *)
(* we can use insert BEFORE defining it *)
and insert elt lst =
  match lst with
  | [] -> [ elt ]
  | h :: tl -> if elt <= h then elt :: lst else h :: insert elt tl

type expression =
  | Const of float
  | Var of string
  | Sum of expression * expression (* e1 + e2 *)
  | Diff of expression * expression (* e1 - e2 *)
  | Prod of expression * expression (* e1 * e2 *)
  | Quot of expression * expression (* e1 / e2 *)

exception Unbound_variable of string

let rec eval env exp =
  match exp with
  | Const x -> x
  | Var v -> (
      try List.assoc v env with Not_found -> raise (Unbound_variable v))
  | Sum (e1, e2) -> eval env e1 +. eval env e2
  | Diff (e1, e2) -> eval env e1 -. eval env e2
  | Prod (e1, e2) -> eval env e1 *. eval env e2
  | Quot (e1, e2) -> eval env e1 /. eval env e2
