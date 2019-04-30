external of_string : string -> bytes = "_mlml_shallow_copy"
external to_string : bytes -> string = "_mlml_shallow_copy"
external create : int -> bytes = "_mlml_create_string"
external _get : bytes * int -> char = "_mlml_get_string"
external _set : (bytes * int -> char) -> unit = "_mlml_set_string"

let get s n = _get (s, n)
let set s n c = _set (s, n, c)
