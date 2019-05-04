external code : char -> int = "_mlml_identity"
external chr : int -> char = "_mlml_identity"

(* simplified version *)
let escaped = function
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | c -> (* TODO: Use String.make 1 c *)
         Bytes.make 1 c |> Bytes.to_string
;;

let lowercase_ascii c = match c with 'A' .. 'Z' -> chr (code c + 32) | _ -> c
let uppercase_ascii c = match c with 'a' .. 'z' -> chr (code c - 32) | _ -> c
