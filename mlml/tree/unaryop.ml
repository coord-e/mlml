type t =
  | Positate
  | Negate

let string_of_unaryop = function Positate -> "+" | Negate -> "-"
