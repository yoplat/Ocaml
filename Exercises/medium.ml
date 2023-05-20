type 'a node = One of 'a | Many of 'a node list

let rec flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many x :: t -> aux (aux acc x) t
  in
  List.rev (aux [] lst)

(* Eliminates consecutive duplicates *)
let rec compress = function
  | h :: (m :: _ as t) -> if h = m then compress t else h :: compress t
  | elm -> elm

(* Creates sublists for consecutive duplicate elements*)
let pack lst =
  let rec aux current acc = function
    | [] -> []
    | [ h ] -> (h :: current) :: acc
    | h :: (m :: _ as t) ->
        if h = m then aux (h :: current) acc t
        else aux [] ((h :: current) :: acc) t
  in
  List.rev (aux [] [] lst)

let encode lst =
  let rec aux n acc = function
    | [] -> []
    | [ h ] -> (n + 1, h) :: acc
    | h :: (m :: _ as t) ->
        if h = m then aux (n + 1) acc t else aux 0 ((n + 1, h) :: acc) t
  in
  List.rev (aux 0 [] lst)

let encode' lst = List.map (fun l -> (List.length l, List.hd l)) (pack lst)

type 'a rle = On of 'a | Man of int * 'a

let encode'' lst =
  let create_tuple cnt elm = if cnt = 0 then On elm else Man (cnt + 1, elm) in
  let rec aux n acc = function
    | [] -> []
    | [ h ] -> create_tuple n h :: acc
    | h :: (m :: _ as t) ->
        if h = m then aux (n + 1) acc t else aux 0 (create_tuple n h :: acc) t
  in
  List.rev (aux 0 [] lst)

let rec create_dup elm = function
  | 1 -> [ elm ]
  | cnt -> elm :: create_dup elm (cnt - 1)

let rec decode lst =
  match lst with
  | [] -> []
  | On elm :: t -> elm :: decode t
  | Man (cnt, elm) :: t -> create_dup elm cnt @ decode t

let rec dup_elem_list = function
  | [] -> []
  | h :: t -> h :: h :: dup_elem_list t

let rec dup_n_elem n = function
  | [] -> []
  | h :: t -> create_dup h n @ dup_n_elem n t

(* Drops every nth element *)
let drop lst n =
  let rec aux k = function
    | [] -> []
    | h :: t -> if k = n then aux 1 t else h :: aux (k + 1) t
  in
  aux 1 lst

let split lst k =
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | h :: t ->
        if i = k then (List.rev (h :: acc), t) else aux (h :: acc) (i + 1) t
  in
  aux [] 1 lst

let slice lst j k =
  let rec aux i = function
    | [] -> []
    | h :: t -> if i >= j && i <= k then h :: aux (i + 1) t else aux (i + 1) t
  in
  aux 0 lst

let rotate lst n =
  let rec aux i = function
    | [] -> []
    | h :: t as l -> if i = 0 then l else aux (i - 1) (t @ [ h ])
  in
  aux n lst

let range low high =
  let rec aux i = if i <= high then i :: aux (i + 1) else [] in
  aux low

let rand_select lst n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t ->
        if n = 0 then (h, List.rev acc @ t) else extract (h :: acc) (n - 1) t
  in
  let rand_extract lst len = extract [] (Random.int len) lst in
  let rec aux n acc lst len =
    if n = 0 then acc
    else
      let pick, rest = rand_extract lst len in
      aux (n - 1) (pick :: acc) rest (len - 1)
  in
  aux n [] lst (List.length lst)

let lotto_select n max = rand_select (range 1 max) n
let permutation lst = rand_select lst (List.length lst)

let rec extract k list =
  if k <= 0 then [ [] ]
  else
    match list with
    | [] -> []
    | h :: t ->
        let with_h = List.map (fun l -> h :: l) (extract (k - 1) t) in
        let without_h = extract k t in
        with_h @ without_h

let length_sort lst =
  List.sort (fun l1 l2 -> List.length l1 - List.length l2) lst

let is_prime n =
  let rec aux i =
    if i < n && n mod i <> 0 then aux (i + 1)
    else if i >= n then true
    else false
  in
  aux 2

let factors n =
  let rec aux acc i k =
    if i < k && k mod i <> 0 then aux acc (i + 1) k
    else if i < k then aux (i :: acc) 2 (k / i)
    else k :: acc
  in
  List.sort Stdlib.compare (aux [] 2 n)

let all_primes low high = List.filter (fun n -> is_prime n) (range low high)

(* For every number there are two primes that sum to it *)
let goldbach n =
  let rec aux d =
    if is_prime d && is_prime (n - d) then (d, n - d)
    else if d <= n then aux (d + 1)
    else failwith "Non esite"
  in
  aux 2
