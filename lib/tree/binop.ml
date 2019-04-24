type t =
  | Add
  | Sub
  | Mul
  | Follow
  | Equal
  | NotEqual
  | PhysicalEqual
  | NotPhysicalEqual
  | Cons
  | StringIndex
  | StringAppend

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Follow -> ";"
  | Equal -> "="
  | NotEqual -> "<>"
  | PhysicalEqual -> "=="
  | NotPhysicalEqual -> "!="
  | Cons -> "::"
  | StringIndex -> ".[]"
  | StringAppend -> "^"
;;
