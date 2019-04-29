external print_int : int -> unit = "_mlml_print_int"
external print_char : char -> unit = "_mlml_print_char"
external print_string : string -> unit = "_mlml_print_string"
external int_of_char : char -> int = "_mlml_identity"
external char_of_int : int -> char = "_mlml_identity"

let not c = if c then false else true
let ( <> ) a b = not (a = b)
let ( == ) a b = not (a != b)
