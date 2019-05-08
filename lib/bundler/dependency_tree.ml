type 'a t =
  | Root of 'a t list
  | Node of 'a * 'a t list

let merge a b =
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

let rec collapse_list l = List.map collapse l |> List.fold_left merge []

and collapse = function
  | Node (name, []) -> [name]
  | Node (name, l) -> name :: collapse_list l
  | Root l -> collapse_list l
;;
