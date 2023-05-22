module type X = sig
  val x : int
end

(* Functor *)
(* The resulting module can do ANYTHING with the module M,
   it doesnt need to return a struct *)
module IncX (M : X) = struct
  let x = M.x + 1
  let y = M.x + 5
end

module A : X = struct
  let x = 0
end

module B = IncX (A)

let _ = print_int B.y

module type StackSig = sig
  type 'a t

  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
end

(* Can test any implementation of StackSig *)
module StackTester (S : StackSig) = struct
  assert (S.(empty |> push 1 |> peek) = 1)
end
