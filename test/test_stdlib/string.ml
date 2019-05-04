external length : string -> int = "_mlml_length_string"
external _get : string * int -> char = "_mlml_get_string"

let get s n = _get (s, n)
let make n c = Bytes.make n c |> Bytes.to_string
let init n c = Bytes.init n c |> Bytes.to_string
let blit = Bytes.blit_string
let sub s = Bytes.sub_string (Bytes.of_string s)

let map f str =
  let aux i = f str.[i] in
  init (length str) aux
;;

let mapi f str =
  let aux i = f i str.[i] in
  init (length str) aux
;;

let rec concat d = function [] -> "" | h :: t -> h ^ d ^ concat d t

(* does not exists in official stdlib *)
let get_opt str i = if length str > i then Some str.[i] else None

let index_opt str chr =
  let rec aux i =
    match get_opt str i with
    | Some c when c = chr -> Some i
    | Some _ -> aux (i + 1)
    | None -> None
  in
  aux 0
;;

let split_on_char c str =
  let rec aux str =
    match index_opt str c with
    | Some i ->
      let before = sub str 0 i in
      let p = i + 1 in
      let after = sub str p (length str - p) in
      let l = aux after in
      before :: l
    | None -> [str]
  in
  aux str
;;

let uppercase_ascii = map Char.uppercase_ascii
let lowercase_ascii = map Char.lowercase_ascii

let apply_hd f s =
  let aux = function 0 -> f s.[0] | i -> s.[i] in
  init (length s) aux
;;

let capitalize_ascii = apply_hd Char.uppercase_ascii
let uncapitalize_ascii = apply_hd Char.lowercase_ascii

let escaped str =
  let rec aux acc i =
    let e = Char.escaped str.[i] in
    let acc = e ^ acc in
    match i with 0 -> acc | i -> aux acc (i - 1)
  in
  aux "" (length str - 1)
;;
