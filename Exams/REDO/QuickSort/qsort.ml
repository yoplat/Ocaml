(* Normal implementation of quick sort algorithm *)

(** Requires the compare function [:>]. *)
let qsort ( >: ) l =
  let rec qsort = function
    | [] -> []
    | h :: tl ->
        qsort (List.filter (fun x -> h >: x) tl)
        @ [ h ]
        @ qsort (List.filter (fun x -> x >: h) tl)
  in
  qsort l
