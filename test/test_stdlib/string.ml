external length : string -> int = "_mlml_length_string"
external _get : bytes * int -> char = "_mlml_get_string"

let get s n = _get (s, n)
let make n c = Bytes.make n c |> Bytes.to_string
let init n c = Bytes.init n c |> Bytes.to_string
let blit = Bytes.blit_string
