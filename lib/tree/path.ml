type t = Path of string list

let string_of_path = function Path l -> String.concat "." l
let path_of_string s = Path (String.split_on_char '.' s)
let of_list l = Path l
let single s = of_list [s]
let length = function Path l -> List.length l

(* return a list of strings from path *)
let extract = function Path l -> l

(* return the common part of two paths                               *)
(* e.g. common (path_of_string "A.B.C") (path_of_string "A.B.ab.cd") *)
(*      -> Path ("A.B")                                              *)
let common a b =
  let rec aux a b =
    match a, b with h1 :: t1, h2 :: t2 when h1 = h2 -> h1 :: aux t1 t2 | _ -> []
  in
  of_list (aux (extract a) (extract b))
;;

(* join two paths *)
let join a b = of_list (extract a @ extract b)

(* check if b is under a *)
let is_under a b =
  let c = common a b in
  c = a
;;

let is_root = function Path [] -> true | _ -> false
let is_empty = is_root
let root = of_list []

(* slow operation: extract the last element of path *)
let init_last path =
  let rec aux = function
    | [h] -> [], h
    | h :: t ->
      let acc, last = aux t in
      h :: acc, last
    | _ -> failwith "Empty"
  in
  aux @@ extract path
;;

let init path = init_last path |> fst
let last path = init_last path |> snd
let last_path path = single @@ last path
let head_tail = function Path (h :: t) -> h, t | Path [] -> failwith "Empty"
let head path = head_tail path |> fst
let tail path = head_tail path |> snd
let compare = compare
let is_capitalized path = match (last path).[0] with 'A' .. 'Z' -> true | _ -> false
let is_single path = length path == 1

let subpaths path =
  let rec aux path =
    match length path with
    | 0 -> [root]
    | _ ->
      let t, _ = init_last path in
      let acc = aux (of_list t) in
      path :: acc
  in
  aux path
;;
