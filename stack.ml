(* You can write the sig in a .mli file and the struct in a .ml
   WITHOUT the module/sig/struct keywords and then compile them together
   - ocamlbuild struct.cmo sig.cmi
   (Builds them in the _build directory)
   In utop :
   #directory "_build" (THIS)
   #load "struct.cmo"  (AND THIS)
   can be put in a .ocmalinit file in the directory to be always executed in a new utop session
   (YOU REFER TO THE OBJECTS AS Stack NOT ListStack)*)

(* #require "ExternalModule" to import external module *)

(* NOTARE che nei commenti si usa 'is', perche' non viene trasformato niente
   vengono tutti construiti ogni volta *)
(* ! Note how is module TYPE when you define a sig *)
module type Stack = sig
  (* You can include another module if you just want to add a functionality *)
  (* You need to do that also in the implementation *)
  (* include OtherModule *)
  (* DIFFERENT from open that just makes the sig available but doesnt copy them *)

  type 'a stack
  (** ['a stack] is the rapresentation of the stack *)

  val empty : 'a stack
  (** [empty] is the empty stack *)

  val push : 'a -> 'a stack -> 'a stack
  (** [push x s] is [s] with [x] pushed on the top*)

  val peek : 'a stack -> 'a
  (** [peek s] is the top element of [s].
      Raises [Failure] if [s] is empty *)

  val pop : 'a stack -> 'a stack
  (** [pop s] is all but the top element of [s].
      Raises [Failure] if [s] is empty *)
end

(* Implementation *)
module ListStack : Stack = struct
  type 'a stack = 'a list

  let empty = []
  let push x s = x :: s
  let peek = function [] -> failwith "Empty" | h :: _ -> h
  let pop = function [] -> failwith "Empty" | _ :: t -> t
end

(* To assign a module implementation to a sig if not done
   module ListStack : Stack but only
   module ListStack = struct *)
module ListFinal : Stack = ListStack

(* You can see the comments on the functions *)
let st = ListStack.(empty |> push 5 |> push 6 |> pop |> peek)
