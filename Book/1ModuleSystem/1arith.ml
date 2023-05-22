module type Arith = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t
  val to_string : t -> string
end

module Ints = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end

module IntsAbstracted : Arith = Ints

(* Exposes that type t = int in repl AND lets integers
   be used as t type *)
(* IntsExposed.(1 + 1) => 2 *)
(* IntsExposed.(one + one) => 2 *)
module IntsExposed : Arith with type t = int = Ints
