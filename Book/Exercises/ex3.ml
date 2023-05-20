let lst = [ 1; 2; 3; 4; 5 ]
let lst = [ 1; 2 ] @ [ 3; 4; 5 ]

(* range function *)
let rec ( -- ) i j = if i < j then i :: (i + 1 -- j) else []

let lst_prod lst =
  let rec aux acc = function [] -> acc | h :: tl -> aux (acc * h) tl in
  aux 1 lst

let concat_lst_string lst = List.fold_right (fun s acc -> s ^ acc) lst ""
let concat_lst_string' lst = List.fold_left (fun acc s -> acc ^ s) "" lst

let rec fold_left f init = function
  | [] -> init
  | h :: tl -> fold_left f (f init h) tl

let rec fold_right f lst init =
  match lst with [] -> init | h :: tl -> f h (fold_right f tl init)

let lst_is_two_or_four = function
  | [ _; _ ] | [ _; _; _; _ ] -> true
  | _ -> false

let eq_first_two = function x :: y :: tl when x = y -> true | _ -> false
let fifth_elm lst = try List.nth lst 4 with Failure _ -> 0
let sort_lst lst = List.sort Stdlib.compare lst

(** Returns last element of a list *)
let last lst =
  try List.nth lst (List.length lst - 1)
  with Invalid_argument _ -> failwith "Empty list"

let has_zeros lst = List.exists (fun x -> x = 0) lst
let rec take n = function h :: tl when n > 0 -> h :: take (n - 1) tl | _ -> []
let rec drop n = function h :: tl when n > 0 -> drop (n - 1) tl | lst -> lst
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

let take_tail n lst =
  let rec aux acc n = function
    | h :: tl when n > 0 -> aux (h :: acc) (n - 1) tl
    | _ -> List.rev acc
  in
  aux [] n lst

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec foldtree f acc = function
  | Leaf -> acc
  | Node (v, l, r) -> f v (foldtree f acc l) (foldtree f acc r)

(** Applies a function twice *)
let twice f x = f (f x)

(** Applies a function [f] [n] times to the item [x] *)
let rec repeat f n x = if n > 0 then repeat f (n - 1) (f x) else x

let product_left = List.fold_left ( *. ) 1.0
let product_right lst = List.fold_right ( *. ) lst 1.0

(** Clip number into range 0 -- 10*)
let clip n = if n < 0 then 0 else if n > 10 then 10 else n

let clip_list = List.map clip
let rec clip_list' = function [] -> [] | x :: tl -> clip x :: clip_list' tl

let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

let rec exists_rec p = function
  | [] -> false
  | h :: tl -> p h || exists_rec p tl

let exists_fold p lst = List.fold_left (fun acc x -> p x || acc) false lst

(** Use a curried function (no tuple used) as one uncurried (with tuple). 
 By partially applicating the function with just the curried function you get 
 the corresponing uncurried function *)
let uncurry f (a, b) = f a b

(** Inverse of uncurry function *)
let curry f a b = f (a, b)

let is_valid_matrix m =
  match m with
  | [] -> false
  | lst ->
      let len_list = List.map List.length lst in
      let length = List.hd len_list in
      List.for_all (fun len -> len = length) len_list

let add_row_vectors = List.map2 ( + )
let add_matrices = List.map2 add_row_vectors
