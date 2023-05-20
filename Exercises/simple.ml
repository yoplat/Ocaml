let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs

let rec at lst n =
  match lst with
  | [] -> failwith "nth"
  | x :: xs -> if n = 0 then Some x else at xs (n - 1)

let length lst =
  let rec length_aux acc = function
    | [] -> acc
    | _ :: xs -> length_aux (acc + 1) xs
  in
  length_aux 0 lst

let rec rev = function [] -> [] | x :: xs -> rev xs @ [ x ]

let rev' lst =
  let rec rev_aux acc = function
    | [] -> acc
    | x :: xs -> rev_aux (x :: acc) xs
  in
  rev_aux [] lst

let is_palindrome lst = lst = List.rev lst
