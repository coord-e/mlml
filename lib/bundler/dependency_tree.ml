type 'a node = Node of 'a * 'a node list
type 'a root = Root of 'a node list

let merge_list a b =
  let rec aux acc = function
    | h :: t ->
      let f x = x <> h in
      let t = List.filter f t in
      aux (h :: acc) t
    | [] -> acc
  in
  let l = List.append a b |> List.rev in
  aux [] l
;;

let rec collapse_list l = List.map collapse_node l |> List.fold_left merge_list []

and collapse_node = function
  | Node (name, []) -> [name]
  | Node (name, l) -> name :: collapse_list l
;;

let collapse = function Root l -> collapse_list l
