type t = Path of string list

let string_of_path = function Path l -> String.concat "." l
let path_of_string s = Path (String.split_on_char '.' s)

(* return a list of strings from path *)
let extract = function Path l -> l

(* return the common part of two paths                               *)
(* e.g. common (path_of_string "A.B.C") (path_of_string "A.B.ab.cd") *)
(*      -> Path ("A.B")                                              *)
let common a b =
  let rec aux a b =
    match a, b with h1 :: t1, h2 :: t2 when h1 = h2 -> h1 :: aux t1 t2 | _ -> []
  in
  aux (extract a) (extract b)
;;

(* join two paths *)
let join a b = Path (extract a @@ extract b)

(* check if b is under a *)
let is_under a b =
  let c = common a b in
  c = a
;;
