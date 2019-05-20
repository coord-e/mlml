let is_relative path = path.[0] <> '/'
let concat a b = a ^ "/" ^ b

let chop_suffix path suff =
  let len = String.length path - String.length suff in
  String.sub path 0 len
;;

let check_suffix path suff =
  let len_suff = String.length suff in
  let len = String.length path - len_suff in
  String.sub path len len_suff = suff
;;

let basename path =
  match String.split_on_char '/' path with
  | [] -> "."
  | l -> List.nth l (List.length l - 1)
;;

let dirname path =
  match String.split_on_char '/' path with
  | [] | [_] -> "."
  | l -> List.nth l (List.length l - 2)
;;
