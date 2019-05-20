type 'a t =
  | Node of 'a * 'a t list
  | Submodule of string * 'a t list

type 'a dep =
  | Entry of 'a
  | Scoped of string * 'a dep_list

and 'a dep_list = 'a dep list

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

let rec collapse_list l = List.map collapse l |> List.fold_left merge_list []

and upper_lower l =
  let aux = function Node (file, _) -> file | Submodule (name, _) -> name in
  let under_names = List.map aux l in
  let l = collapse_list l in
  let f = function Entry name | Scoped (name, _) -> List.mem name under_names in
  List.partition f l

and collapse = function
  | Node (file, []) -> [Entry file]
  | Node (file, l) -> Entry file :: collapse_list l
  | Submodule (name, l) ->
    (* `upper` is what is expected to be accessible as if it is under the name *)
    (* `lower` is the other, non-related things                                *)
    let upper, lower = upper_lower l in
    Scoped (name, upper) :: lower
;;
