type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval val_var = function
  | Var x -> List.assoc x val_var
  | Not e -> not (eval val_var e)
  | And (e1, e2) -> eval val_var e1 && eval val_var e2
  | Or (e1, e2) -> eval val_var e1 && eval val_var e2

let rec table_make val_var vars expr =
  match vars with
  | [] -> [ (List.rev val_var, eval val_var expr) ]
  | h :: t ->
      table_make ((h, true) :: val_var) t expr
      @ table_make ((h, false) :: val_var) t expr

let table vars expr = table_make [] vars expr