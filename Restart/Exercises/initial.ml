let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs

let rec at k = function
  | [] -> raise (Failure "nth")
  | x :: xs -> if k = 0 then x else at (k - 1) xs

let length_tail l =
  let rec aux acc = function [] -> acc | _ :: tl -> aux (acc + 1) tl in
  aux 0 l

let rev l =
  let rec aux acc = function [] -> acc | h :: tl -> aux (h :: acc) tl in
  aux [] l

type 'a node = One of 'a | Many of 'a node list

let flatten_nodes ls =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many xs :: t -> aux (aux acc xs) t
  in
  List.rev aux [] ls
