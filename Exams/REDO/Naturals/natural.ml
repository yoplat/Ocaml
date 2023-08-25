module Natural = struct
  type natural = Zero | Succ of natural

  exception NegativeNumber
  exception DivisionByZero

  let rec eval = function Zero -> 0 | Succ x -> succ (eval x)

  let convert n =
    let rec convert r n = if 0 < n then convert (Succ r) (pred n) else r in
    convert Zero n

  let rec ( + ) n1 = function Zero -> n1 | Succ n -> Succ n1 + n

  let rec ( > ) a b =
    match (a, b) with
    | Succ _, Zero -> true
    | Zero, Succ _ | Zero, Zero -> false
    | Succ n, Succ m -> n > m

  let rec ( - ) a b =
    if not (b > a) then
      (* If a is >= then b*)
      match (a, b) with
      | n1, Zero -> n1
      | Succ n1, Succ n2 -> n1 - n2
      | Zero, Succ _ -> failwith "Impossible to reach"
    else raise NegativeNumber

  let ( * ) a b =
    if b > Zero then
      let rec ( * ) r n = function
        | Zero -> Zero (* Should never be reached *)
        | Succ Zero -> r
        | Succ x -> ( * ) (r + n) n x
      in
      ( * ) a a b
    else Zero

  let ( / ) a b =
    if a > Zero then
      if not (b > Zero) then raise DivisionByZero
      else
        let rec ( / ) r a b =
          if not (b > a) then ( / ) (Succ r) (a - b) b else r
        in
        ( / ) Zero a b
    else Zero
end

module N : NaturalI.NaturalI = Natural
