module type X = sig
  val x : int
end

module A : X = struct
  let x = 0
end

(* FUNCTORS are a way to perform a function on a module *)
(* Functors take in a module sig and output another module *)
module IncX =
functor
  (M : X)
  ->
  struct
    (* Module with x incremented by one *)
    let x = M.x + 1
  end

(* The above is equal to writing *)
module IncX (M : X) = struct
  let x = M.x + 1
end

(* Module with x = 1 *)
module B = IncX (A)

(* Module with x = 2 *)
module C = IncX (B)

type day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

let int_of_day = function
  | Monday -> 0
  | Tuesday -> 1
  | Wednesday -> 2
  | Thursday -> 3
  | Friday -> 4
  | Saturday -> 5
  | Sunday -> 6

module DayKey = struct
  (* The Map module (Dictionaries not map function) requires
     to make a new type of map key
     the type t of the key
     and a compare function *)
  type t = day

  let compare day1 day2 = int_of_day day1 - int_of_day day2
end

(* Making a new Map module using Map.Make *)
module DayMap = Map.Make (DayKey)

(* Now we can use all the Map. functions provided by the Stdlib *)
let x = DayMap.empty
let y = DayMap.add Monday "Lunedi" x
