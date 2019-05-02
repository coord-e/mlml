type t =
  | Add
  | Sub
  | Mul
  | Follow
  | Equal
  | NotPhysicalEqual
  | Lt
  | Gt
  | And
  | Or
  | Cons
  | StringIndex
  | ArrayIndex
  | Mod
  | Div
  (* non-keyword operators *)
  | Custom of string

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Gt -> ">"
  | Mod -> "mod"
  | Follow -> ";"
  | Equal -> "="
  | NotPhysicalEqual -> "!="
  | And -> "&&"
  | Or -> "||"
  | Cons -> "::"
  | StringIndex -> ".[]"
  | ArrayIndex -> ".()"
  | Custom s -> s
;;
