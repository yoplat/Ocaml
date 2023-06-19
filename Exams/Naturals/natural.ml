module Natural = struct
  type natural = Zero | Succ of natural
  exception NegativeNumber
  exception DivisionByZero

  let rec ( + ) n = function 
    Zero -> n 
    | Succ m -> ( + ) (Succ n) m
  
  let (>) a b = 
    match a, b with 
    | Succ _, Zero -> true 
    | Zero, Succ _ | Zero, Zero -> false
    | Succ n, Succ m -> (>) n m 
  
  let rec ( - ) n m = 
    if not (m > n) then
      match n, m with 
      | n', Zero -> n'
      | Succ(n'), Succ(m') -> ( - ) n' m'
    else raise NegativeNumber
  
  let ( * ) n m = 
    if n > Zero then 
      let rec ( * ) r n = function 
        | Zero -> Zero 
        | Succ(Zero) -> r 
        | Succ(m) -> ( * ) (( + ) r n) n m  
      in ( * ) n n m 
    else Zero

  let ( / ) n m = 
    if n > Zero then 
      if m > Zero then 
        let rec ( / ) r n m = 
          if not (m > n) then ( / ) (Succ r) ((-) n m) m
          else r 
        in ( / ) Zero n m 
      else raise DivisionByZero
    else Zero

  let rec eval = function 
    | Zero -> 0
    | Succ x -> succ (eval x)

  let convert n = 
    let rec convert r n =
      if 0 < n then convert (Succ r) (pred n)
      else r 
    in convert Zero n
end

module N = (Natural: NaturalI.NaturalI)
