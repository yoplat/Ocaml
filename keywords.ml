(* To compile: ocamlc -o output_name file_name
   '#use file_name.ml' to import file in a ocaml REPL *)

(* ! 'rlwrap ocaml' for exam *)

(* To compile modules:
    - ocamlc -c file_sig.mli
    - ocmalc -c file_struct.ml
    ! the two above can have the same file name (apart from extension)
    - ocaml -o main compiled.cmo main.ml *)

(* UTOP in CMD line to use REPL *)

(* IMPORTANTTTTTTTTTTTTTT *)
(* #trace fun_name
   and then calling that function prints all the calls and returned values *)

(* Statements always end with a ;; *)
let invert x = match x with true -> false | false -> true;;

(* Function application to avoid putting parenthesis on the right *)
(invert @@ true) && false;;

(* Inverse application *)
true && false |> invert;;

(* String concatenation *)
"ciao io sono" ^ "Carlo"

(* For nth function *)
open List

let rec fibo n = if n <= 1 then n else fibo (n - 1) + fibo (n - 2)

(* fun_name() means that the function is void *)
let main () =
  (* 'in' keyword used to declare the use of a local variable in a piece of code*)
  let in's = [ 5; 7; 15; 25; 30 ] in
  for i = 0 to List.length in's - 1 do
    (* nth gets the nth element in a collection (list) *)
    print_endline
      ("fibo("
      ^ string_of_int (nth in's i)
      ^ "): "
      ^ string_of_int (fibo (nth in's i)))
  done

(* Accessing tuple components ONLY FOR PAIRS *)
let tuple = (1, 5);;

fst tuple;;
snd tuple

(* '*.' used for float multiplication (add . after normal sign to indicate float op) *)
let prodFloat x y = x *. y

(* Resto di una divisione *)
let remainder x y = x mod y
let parola = "ciao";;

(* Indexing strings char *)
parola.[0]

let slice i j xs =
  let rec slice count res = function
    | y :: ys when count < i -> slice (count + 1) res ys
    | _ when count == j -> List.rev res
    | y :: ys -> slice (count + 1) (y :: res) ys
    | [] -> []
  in
  slice 0 [] xs

(* Arrays are different from lists, they are omogeneus, mutable and direct accessible *)
let arr = [| 1; 2; 4; 7; 9 |];;

(* Indexing an array and changing value *)
arr.(2) <- 5

(* Useful functions:
   Array.make n_values value
   Array.concat [first; second]
   Array.make_matrix N M value *)
(* Useful list functions:
   List.mem x arr to check if contains *)

(* Records *)
type person = { name : string; mutable age : int; gender : string }

let p = { name = "Walter"; age = 35; gender = "M" };;

p.age <- p.age + 1

(* Record copy *)
let q = { p with name = "Jon"; age = 14 }

(* p.name <- "Carlo";; -> Error non mutable *)

(* CHECK MODULE IMPLEMENTATIONS *)

(* List in ocaml *)
(* Generic types *)
type 'a mylist = [] | ( :: ) of 'a * 'a mylist

let head lst =
  match lst with
  (* Raising an exception *)
  | [] -> raise (Invalid_argument "Empty list")
  | x :: _ -> x

(* use begin and end to wrap a match inside another *)
(* IMPORTANT *)
let rec list_max = function
  | h :: t -> (
      match list_max t with None -> Some h | Some x -> Some (max h x))
  | [] -> None

(* EXCEPTIONS *)
exception OhNo of string;;

raise (OhNo "ciao");;

(* Raises Failure exception *)
failwith "Eccezione"

(* outputs x / y if y not 0 else returns 0 *)
let save_div x y = try x / y with Division_by_zero -> 0

let filter p lst =
  let rec filter_aux acc = function
    (* Using List.rev to reverse the accumulator at the end *)
    | [] -> List.rev acc
    | h :: t -> filter_aux (if p h then p :: acc else acc) t
  in
  filter_aux [] lst
;;

(* Option.map maps the value of an option given a function *)
Option.map (fun x -> x + 1) (Some 5);;

(* To avoid having double Some : Some (Some x) by concatenating function *)
(* You have a function that wants as input a non-option value but returns an Option *)
(* If you already have the option value you can use the bind operator *)
Option.bind (Some 5) (fun x -> Some (x + 1))

(* Creates a sorted set (removes duplicates) *)
let set = [ 1; 2; 4; 4; 7; 9; 7 ] |> List.sort_uniq Stdlib.compare
