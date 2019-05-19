type 'a t =
  | Node of 'a * 'a t list
  | Submodule of string * 'a t list

type 'a dep =
  | Entry of 'a
  | Scoped of string * 'a dep_list

and 'a dep_list = 'a dep list

let merge_list a b =
  let rec part_deep f = function
    | [] -> [], []
    | h :: t ->
      let a, b = part_deep f t in
      (match h with
      | Entry p when f p -> h :: a, b
      | Entry _ -> a, h :: b
      | Scoped (name, l) ->
        let a', b' = part_deep f l in
        Scoped (name, a') :: a, Scoped (name, b') :: b)
  in
  let filter_out f l = part_deep f l |> fst in
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

and collapse = function
  | Node (file, []) -> [Entry file]
  | Node (file, l) -> Entry file :: collapse_list l
  | Submodule (name, l) -> [Scoped (name, collapse_list l)]
;;
