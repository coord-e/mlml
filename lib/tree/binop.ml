type 'a t =
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
  | Custom of 'a

let string_of_binop f = function
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
  | Custom s -> f s
;;

let apply_on_custom f = function
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Follow -> Follow
  | Equal -> Equal
  | NotPhysicalEqual -> NotPhysicalEqual
  | Lt -> Lt
  | Gt -> Gt
  | And -> And
  | Or -> Or
  | Cons -> Cons
  | StringIndex -> StringIndex
  | ArrayIndex -> ArrayIndex
  | Mod -> Mod
  | Div -> Div
  | Custom s -> Custom (f s)
;;
