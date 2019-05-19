type 'a t =
  | Node of 'a * 'a t list
  | Submodule of string * 'a t list

type 'a dep =
  | Entry of 'a
  | Scoped of string * 'a dep_list

and 'a dep_list = 'a dep list

let merge_list a b =
  let rec filter_out f = function
    | [] -> []
    | h :: t ->
      (match h with
      | Entry p when f p -> h :: filter_out f t
      | Entry _ -> filter_out f t
      | Scoped (name, l) -> Scoped (name, filter_out f l) :: filter_out f t)
  in
  let rec aux acc = function
    | h :: t ->
      let t =
        match h with
        | Entry p ->
          let f x = x <> p in
          filter_out f t
        | Scoped _ -> t
      in
      aux (h :: acc) t
    | [] -> acc
  in
  let l = List.append a b |> List.rev in
  aux [] l
;;

let rec collapse_list l = List.map collapse l |> List.fold_left merge_list []

and upper_lower l =
  let aux = function Node (file, l) -> [Entry file], l | t -> collapse t, [] in
  let upper, lower = List.map aux l |> List.split in
  List.flatten upper, List.flatten lower

and collapse = function
  | Node (file, []) -> [Entry file]
  | Node (file, l) -> Entry file :: collapse_list l
  | Submodule (name, l) ->
    let upper, lower = upper_lower l in
    Scoped (name, upper) :: collapse_list lower
;;
