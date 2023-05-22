module type Stack = sig
  type 'a t (* It's better to call the type t for better output *)

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
end

module ListStack : Stack = struct
  type 'a t = 'a list

  let empty = []
  let is_empty s = s = []
  let push x s = x :: s
  let peek = function [] -> failwith "Empty" | x :: _ -> x
  let pop = function [] -> failwith "Empty" | _ :: xs -> xs
end

module MyStack : Stack = struct
  type 'a t = Empty | Entry of 'a * 'a t

  let empty = Empty
  let is_empty s = s = Empty
  let push x s = Entry (x, s)
  let peek = function Empty -> failwith "Empty" | Entry (x, _) -> x
  let pop = function Empty -> failwith "Empty" | Entry (_, s) -> s
end
