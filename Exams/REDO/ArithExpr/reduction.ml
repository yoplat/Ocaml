module ArithExpr = struct
  type expr =
    | Sum of expr * expr
    | Dif of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Number of float

  let rec tostring = function
    | Number n -> string_of_float n
    | Sum (e1, e2) ->
        String.concat " " [ "("; tostring e1; "+"; tostring e2; ")" ]
    | Dif (e1, e2) ->
        String.concat " " [ "("; tostring e1; "-"; tostring e2; ")" ]
    | Mul (e1, e2) ->
        String.concat " " [ "("; tostring e1; "*"; tostring e2; ")" ]
    | Div (e1, e2) ->
        String.concat " " [ "("; tostring e1; "/"; tostring e2; ")" ]

  let rec combine = function
    | Number n -> Number n
    | Sum (Number n1, Number n2) -> Number (n1 +. n2)
    | Sum (n1, n2) -> Sum (combine n1, combine n2)
    | Dif (Number n1, Number n2) -> Number (n1 -. n2)
    | Dif (n1, n2) -> Dif (combine n1, combine n2)
    | Mul (Number n1, Number n2) -> Number (n1 *. n2)
    | Mul (n1, n2) -> Mul (combine n1, combine n2)
    | Div (Number n1, Number n2) -> Number (n1 /. n2)
    | Div (n1, n2) -> Div (combine n1, combine n2)

  let rec print_reduction e =
    print_endline (tostring e);
    match e with Number _ -> () | _ -> print_reduction (combine e)

  let parse str =
    let rec parse str n =
      match str.[n] with
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
          (Number (float_of_string (String.make 1 str.[n])), n)
      | '+' ->
          let op1, n1 = parse str (n + 1) in
          let op2, n2 = parse str (n1 + 1) in
          (Sum (op1, op2), n2)
      | '-' ->
          let op1, n1 = parse str (n + 1) in
          let op2, n2 = parse str (n1 + 1) in
          (Dif (op1, op2), n2)
      | '*' ->
          let op1, n1 = parse str (n + 1) in
          let op2, n2 = parse str (n1 + 1) in
          (Mul (op1, op2), n2)
      | '/' ->
          let op1, n1 = parse str (n + 1) in
          let op2, n2 = parse str (n1 + 1) in
          (Div (op1, op2), n2)
      | _ -> failwith "Illegal argument"
    in
    fst (parse str 0)

  let print_evaluation str = print_reduction (parse str)
end
