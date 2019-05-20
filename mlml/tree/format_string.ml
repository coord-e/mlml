type kind =
  | Const of string
  | Int
  | Char
  | String

let string_of_kind = function
  | Const s -> s
  | Int -> "%d"
  | Char -> "%c"
  | String -> "%s"
;;

let string_of_format_string l = List.map string_of_kind l |> String.concat ""
