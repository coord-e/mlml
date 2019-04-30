external of_string : string -> bytes = "_mlml_shallow_copy"
external to_string : bytes -> string = "_mlml_shallow_copy"
external copy : bytes -> bytes = "_mlml_shallow_copy"
external create : int -> bytes = "_mlml_create_string"
external length : bytes -> int = "_mlml_length_string"
external _get : bytes * int -> char = "_mlml_get_string"
external _set : (bytes * int -> char) -> unit = "_mlml_set_string"

let get s n = _get (s, n)
let set s n c = _set (s, n, c)
let empty = create 0

let init n f =
  let b = create n in
  let rec aux = function
    | 0 -> set b 0 (f 0)
    | i ->
      set b i (f i);
      aux (i - 1)
  in
  aux (n - 1);
  b
;;

let make n c = init n (fun _ -> c)
