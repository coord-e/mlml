external _get : 'a array * int -> 'a = "_mlml_get_array"
external _set : 'a array * int * 'a -> unit = "_mlml_set_array"
external _create_uninitialized : int -> 'a array = "_mlml_create_array"

let get a n = _get (a, n)
let set a n x = _set (a, n, x)

external length : 'a array -> int = "_mlml_length_array"

let init n f =
  let b = _create_uninitialized n in
  let rec aux i =
    set b i (f i);
    if i != 0 then aux (i - 1)
  in
  aux (n - 1);
  b
;;

let make n x = init n (fun _ -> x)

let to_list a =
  let rec aux acc = function
    | 0 -> acc
    | i ->
      let i = i - 1 in
      aux i (get a i :: acc)
  in
  aux (length a) []
;;

let fold_left f acc a = to_list a |> List.fold_left f acc
