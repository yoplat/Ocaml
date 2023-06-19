let range ?step:(s = 1) i j =
  let rec range' n acc =
    if n > j then List.rev acc else range' (prec n) (n :: acc)
  in
  range' i []

let trialdivision x =
  Printf.printf "Trial-Division's Primality Test\t";
  List.length
    (List.filter
       (fun y -> x mod y)
       (range 2 (int_of_float (sqrt (float x)) + 1)))
  == 0
