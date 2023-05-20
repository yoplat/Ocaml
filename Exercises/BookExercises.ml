let take n ls =
  let rec aux i acc = function
    | [] -> List.rev acc
    | h :: t -> if i < n then aux (i + 1) (h :: acc) t else List.rev acc
  in
  aux 0 [] ls

let drop n ls =
  let rec aux i = function
    | [] -> []
    | h :: t -> if i < n then aux (i + 1) t else t
  in
  aux 1 ls

let createpalin ls = ls @ List.rev ls
let is_palindrome ls = ls = List.rev ls

let droplast ls =
  let rec aux acc = function
    | [] -> []
    | [ x ] -> List.rev acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] ls

let rec member x = function
  | [] -> false
  | y :: xs -> if x = y then true else member x xs

let make_set ls =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> if member x acc then aux acc xs else aux (x :: acc) xs
  in
  aux [] ls

let rev ls =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x :: acc) xs in
  aux [] ls

(** Inserts a comparable element in an already sorted list *)
let rec insert x = function
  | [] -> [ x ]
  | h :: tl as xs -> if x > h then h :: insert x tl else x :: xs

(** Sorts a list in ascending order *)
let rec sort = function [] -> [] | x :: xs -> insert x (sort xs)

let rec is_sorted = function
  | x :: y :: t -> if x < y then is_sorted (y :: t) else false
  | _ -> true

let rec merge x y =
  match (x, y) with
  | [], t -> t
  | t, [] -> t
  | hx :: tx, hy :: ty ->
      if hx < hy then hx :: merge tx (hy :: ty) else hy :: merge (hx :: tx) ty

let rec msort = function
  | [] -> []
  | [ x ] -> [ x ]
  | l ->
      let left = take (List.length l / 2) l in
      let right = drop (List.length l / 2) l in
      merge (msort left) (msort right)

(** Maps over a list of lists *)
let mapl f = List.map (List.map f)

(** Maps over a list of lists of lists *)
let mapll f = List.map (List.map (List.map f))

let rec truncate n ls = if List.length ls > n then take n ls else ls
let truncatel n = List.map (truncate n)

(** Prints a key value pair in nice format *)
let print_entry (k, v) =
  print_int k;
  print_newline ();
  print_string v;
  print_newline ()

let rec iter f = function
  | [] -> ()
  | h :: t ->
      f h;
      iter f t

(** Prints a dictionary *)
let rec print_dic = iter print_entry

(** Reads a dictionary from standard input with "[int_key] [string_value]" format 
    separated by new lines *)
let rec read_dic () =
  try
    let i = read_int () in
    if i = 0 then []
    else
      let v = read_line () in
      (i, v) :: read_dic ()
    (* Should fail only if a non integer is inputted as a key*)
  with Failure _ ->
    print_string "This is not a valid integer, please try again";
    print_newline ();
    read_dic ()

(** Outputs an entry to a channel (can be a file) *)
let entry_to_channel ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'

(** Outputs a dictionary to a channel (can be a file) *)
let dictionary_to_channel ch = iter (entry_to_channel ch)

(** Outputs a dictionary to a file *)
let dictionary_to_file filename dict =
  (* * Corresponding functions for input *)
  let ch = open_out filename in
  dictionary_to_channel ch dict;
  close_out ch

(** Inputs an entry from a channel *)
let entry_of_channel ch =
  let number = input_line ch in
  let name = input_line ch in
  (int_of_string number, name)

(** Pointer to x containing 0 *)
let x = ref 0

(** Extracting the value contained in x *)
let p = !x;;

(* * Changing the value inside x (! p is unchanged) *)
x := 10
