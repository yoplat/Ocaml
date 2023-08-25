(** Continuation version of the quicksort algorithm, so 
 it's tail recursive .*)
let cqsort ( >: ) l =
  let rec cqsort l k =
    match l with
    | [] | [ _ ] -> k l
    | h :: tl ->
        (* NOTE: Tail recursive!!! *)
        cqsort
          (List.filter (fun x -> h >: x) tl)
          (fun l1 ->
            (* Notice how the continuation is a list to list
               function (only ONE arg) *)
            k
              (cqsort
                 (List.filter (fun x -> x >: h) tl)
                 (fun l2 -> l1 @ (h :: l2))))
  in
  cqsort l (fun x -> x)
