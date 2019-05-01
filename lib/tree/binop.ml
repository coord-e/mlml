type t =
  | Add
  | Sub
  | Mul
  | Follow
  | Equal
  | NotPhysicalEqual
  | Cons
  | StringIndex
  | Mod
  | Div
  (* non-keyword operators *)
  | Custom of string

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | Follow -> ";"
  | Equal -> "="
  | NotPhysicalEqual -> "!="
  | Cons -> "::"
  | StringIndex -> ".[]"
  | Custom s -> s
;;
