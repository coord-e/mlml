external _get : 'a array * int -> 'a = "_mlml_get_array"
external _set : 'a array * int * 'a -> unit = "_mlml_set_array"

let get a n = _get (a, n)
let set a n x = _set (a, n, x)
