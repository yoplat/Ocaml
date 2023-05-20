(* Deve iniziare con la maiuscola *)
module MyList = struct
  type 'a myList =
    | Nil
    | Cons of 'a * 'a myList

  let rec map f = function
    | Nil -> Nil
    | Cons (h, t) -> Cons (f h, map f t)
end

module Tree = struct
  type 'a tree = 
    | Leaf 
    | Node of 'a * 'a tree * 'a tree

  let rec map f = function
    | Leaf -> Leaf
    | Node (v, r, l) -> Node (f v, map f r, map f l)
end

(* Modules struct can be used as namespaces *)
let lst = MyList.map succ (Cons (1, Nil));;

(* Putting Tree. before the parenthesis removes the need to put Tree. for every function called inside *)
let tree = Tree.(map succ (Node (1, Leaf, Leaf)));;
(* Another way: *)
let tree' = 
  let open Tree in
  map succ (Node (1, Leaf, Leaf));;
(* Another way not recommended (overshadowing problems for conclicting names)*)
(* open Tree;; *)

(* NOTE THE TYPE KEYWORD to define an interface for other modules *)
(* It defines the ONLY functions or members that can be used from an implementation of the interface*)
(* You CAN'T access other functions defined inside the implementation not defined in the sig *)
module type Fact = sig
  (* The double * indicates a comment for the function (signature to show as doc)*)
  (** [fact n] is [n] factorial. *)
  val fact : int -> int
end

module RecursiveFact : Fact = struct
  let rec fact n = 
    if n = 0 then 1 else n * fact (n-1)
end