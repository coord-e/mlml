type token =
  | IntLiteral of int
  | BoolLiteral of bool
  | CapitalIdent of string
  | LowerIdent of string
  | Plus
  | Minus
  | Star
  | Let
  | Rec
  | In
  | Equal
  | If
  | Then
  | Else
  | LParen
  | RParen

let to_digit c = int_of_char c - int_of_char '0'

let string_of_chars chars =
  let buf = Buffer.create 8 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf
;;

let rec read_int acc rest =
  match rest with
  | h :: t ->
    (match h with '0' .. '9' -> read_int ((acc * 10) + to_digit h) t | _ -> rest, acc)
  | _ -> [], acc
;;

let rec read_ident acc rest =
  match rest with
  | h :: t ->
    (match h with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
      let rest, ident = read_ident acc t in
      rest, h :: ident
    | _ -> rest, acc)
  | _ -> [], acc
;;

let rec tokenize_aux acc rest =
  match rest with
  | [] -> acc
  | h :: t ->
    (match h with
    | ' ' | '\t' | '\n' -> tokenize_aux acc t
    | '0' .. '9' ->
      let rest, num = read_int 0 rest in
      tokenize_aux (IntLiteral num :: acc) rest
    | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
      let rest, ident = read_ident [] rest in
      let ident_str = string_of_chars ident in
      (match ident_str with
      | "let" -> tokenize_aux (Let :: acc) rest
      | "rec" -> tokenize_aux (Rec :: acc) rest
      | "in" -> tokenize_aux (In :: acc) rest
      | "true" -> tokenize_aux (BoolLiteral true :: acc) rest
      | "false" -> tokenize_aux (BoolLiteral false :: acc) rest
      | "if" -> tokenize_aux (If :: acc) rest
      | "then" -> tokenize_aux (Then :: acc) rest
      | "else" -> tokenize_aux (Else :: acc) rest
      | _ ->
        (match ident_str.[0] with
        | 'A' .. 'Z' -> tokenize_aux (CapitalIdent ident_str :: acc) rest
        | _ -> tokenize_aux (LowerIdent ident_str :: acc) rest))
    | '+' -> tokenize_aux (Plus :: acc) t
    | '-' -> tokenize_aux (Minus :: acc) t
    | '*' -> tokenize_aux (Star :: acc) t
    | '=' -> tokenize_aux (Equal :: acc) t
    | '(' -> tokenize_aux (LParen :: acc) t
    | ')' -> tokenize_aux (RParen :: acc) t
    | _ -> failwith @@ Printf.sprintf "unexpected character: '%c'" h)
;;

let string_of_token = function
  | IntLiteral num -> string_of_int num
  | BoolLiteral b -> string_of_bool b
  | CapitalIdent ident | LowerIdent ident -> ident
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Let -> "let"
  | Rec -> "rec"
  | In -> "in"
  | Equal -> "="
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | LParen -> "("
  | RParen -> ")"
;;

let string_of_tokens tokens =
  let aux acc t = string_of_token t ^ ", " ^ acc in
  List.fold_left aux "" @@ List.rev tokens
;;

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let f source = explode source |> tokenize_aux [] |> List.rev