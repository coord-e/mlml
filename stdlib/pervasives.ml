external print_char : char -> unit = "_mlml_print_char"
external print_string : string -> unit = "_mlml_print_string"
external prerr_string : string -> unit = "_mlml_prerr_string"
external int_of_char : char -> int = "_mlml_identity"
external char_of_int : int -> char = "_mlml_identity"
external _append_string : string * string -> string = "_mlml_append_string"

let string_of_bool = function true -> "true" | false -> "false"

let print_endline s =
  print_string s;
  print_char '\n'
;;

let not c = if c then false else true
let ( <> ) a b = not (a = b)
let ( == ) a b = not (a != b)
let ( ^ ) a b = _append_string (a, b)
let ( <= ) a b = not (a > b)
let ( >= ) a b = not (a < b)
let rec ( @ ) a b = match a with [] -> b | h :: t -> h :: (t @ b)

(* TODO: @@ is right-assiciative *)
let ( @@ ) f a = f a
let ( |> ) v f = f v

type 'a option =
  | Some of 'a
  | None

external exit : int -> 'a = "_mlml_exit"

let failwith msg =
  prerr_string msg;
  exit 2
;;

(* TODO: Use -1 *)
let compare a b =
  match a = b, a > b with true, _ -> 0 | _, true -> 1 | _, false -> 0 - 1
;;

let succ x = x + 1
let fst (a, _) = a
let snd (_, b) = b

external open_in : string -> in_channel = "_mlml_open_in"
external close_in : in_channel -> unit = "_mlml_close_in"
external in_channel_length : in_channel -> int = "_mlml_in_channel_length"
external _really_input_string : in_channel * int -> string = "_mlml_really_input_string"

let really_input_string ic len = _really_input_string (ic, len)
let ignore _ = ()
