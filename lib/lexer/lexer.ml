type token =
  | IntLiteral of int
  | BoolLiteral of bool
  | StringLiteral of string
  | CharLiteral of char
  | CapitalIdent of string
  | LowerIdent of string
  | Plus
  | Minus
  | Star
  | Let
  | Rec
  | In
  | And
  | Equal
  | DoubleEqual
  | NotEqual
  | LtGt
  | Lt
  | If
  | Then
  | Else
  | Type
  | Vertical
  | Excl
  | Of
  | Match
  | With
  | When
  | Fun
  | Arrow
  | Function
  | Comma
  | Semicolon
  | DoubleSemicolon
  | Colon
  | DoubleColon
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Dot
  | DoubleDot
  | Hat
  | Apostrophe

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

let read_one_char = function
  | '\\' :: t ->
    (match t with
    | '\\' :: rest -> rest, '\\'
    | '"' :: rest -> rest, '"'
    | '\'' :: rest -> rest, '\''
    | 'n' :: rest -> rest, '\n'
    | 'r' :: rest -> rest, '\r'
    | 't' :: rest -> rest, '\t'
    | 'b' :: rest -> rest, '\b'
    | ' ' :: rest -> rest, ' '
    | _ ->
      failwith "Invalid escape sequence" (* TODO: Implement ASCII escape sequences *))
  | c :: rest -> rest, c
  | [] -> failwith "attempt to read a char from empty input"
;;

let rec read_string acc rest =
  let rest, c = read_one_char rest in
  match c with
  | '"' -> rest, acc
  | c ->
    let rest, acc = read_string acc rest in
    rest, c :: acc
;;

let try_read_char tokens =
  let rest, c = read_one_char tokens in
  match rest with '\'' :: rest -> rest, Some c | _ -> tokens, None
;;

let read_char tokens =
  match try_read_char tokens with
  | rest, Some c -> rest, c
  | _, None -> failwith "invalid char literal"
;;

let rec read_ident acc rest =
  match rest with
  | h :: t ->
    (match h with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
      let rest, ident = read_ident acc t in
      rest, h :: ident
    | _ -> rest, acc)
  | _ -> [], acc
;;

let rec tokenize_aux acc rest =
  match rest with
  | [] -> acc
  | '(' :: '*' :: rest ->
    let rec consume_comment level = function
      | '(' :: '*' :: rest -> consume_comment (level + 1) rest
      | '*' :: ')' :: rest when level = 0 -> rest
      | '*' :: ')' :: rest -> consume_comment (level - 1) rest
      | _ :: t -> consume_comment level t
      | [] -> failwith "comment does not end"
    in
    consume_comment 0 rest |> tokenize_aux acc
  | h :: t ->
    (match h with
    | ' ' | '\t' | '\n' -> tokenize_aux acc t
    | '0' .. '9' ->
      let rest, num = read_int 0 rest in
      tokenize_aux (IntLiteral num :: acc) rest
    | '"' ->
      let rest, str = read_string [] t in
      let str_str = string_of_chars str in
      tokenize_aux (StringLiteral str_str :: acc) rest
    | '\'' ->
      (match try_read_char t with
      | rest, Some ch -> tokenize_aux (CharLiteral ch :: acc) rest
      | rest, None -> tokenize_aux (Apostrophe :: acc) rest)
    | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
      let rest, ident = read_ident [] rest in
      let ident_str = string_of_chars ident in
      (match ident_str with
      | "let" -> tokenize_aux (Let :: acc) rest
      | "rec" -> tokenize_aux (Rec :: acc) rest
      | "in" -> tokenize_aux (In :: acc) rest
      | "and" -> tokenize_aux (And :: acc) rest
      | "true" -> tokenize_aux (BoolLiteral true :: acc) rest
      | "false" -> tokenize_aux (BoolLiteral false :: acc) rest
      | "if" -> tokenize_aux (If :: acc) rest
      | "then" -> tokenize_aux (Then :: acc) rest
      | "else" -> tokenize_aux (Else :: acc) rest
      | "type" -> tokenize_aux (Type :: acc) rest
      | "of" -> tokenize_aux (Of :: acc) rest
      | "match" -> tokenize_aux (Match :: acc) rest
      | "with" -> tokenize_aux (With :: acc) rest
      | "when" -> tokenize_aux (When :: acc) rest
      | "fun" -> tokenize_aux (Fun :: acc) rest
      | "function" -> tokenize_aux (Function :: acc) rest
      | _ ->
        (match ident_str.[0] with
        | 'A' .. 'Z' -> tokenize_aux (CapitalIdent ident_str :: acc) rest
        | _ -> tokenize_aux (LowerIdent ident_str :: acc) rest))
    | '+' -> tokenize_aux (Plus :: acc) t
    | '-' ->
      (match t with
      | '>' :: t -> tokenize_aux (Arrow :: acc) t
      | _ -> tokenize_aux (Minus :: acc) t)
    | '*' -> tokenize_aux (Star :: acc) t
    | '=' ->
      (match t with
      | '=' :: t -> tokenize_aux (DoubleEqual :: acc) t
      | _ -> tokenize_aux (Equal :: acc) t)
    | '<' ->
      (match t with
      | '>' :: t -> tokenize_aux (LtGt :: acc) t
      | _ -> tokenize_aux (Lt :: acc) t)
    | '!' ->
      (match t with
      | '=' :: t -> tokenize_aux (NotEqual :: acc) t
      | _ -> tokenize_aux (Excl :: acc) t)
    | '|' -> tokenize_aux (Vertical :: acc) t
    | ',' -> tokenize_aux (Comma :: acc) t
    | '(' -> tokenize_aux (LParen :: acc) t
    | ')' -> tokenize_aux (RParen :: acc) t
    | '[' -> tokenize_aux (LBracket :: acc) t
    | ']' -> tokenize_aux (RBracket :: acc) t
    | '{' -> tokenize_aux (LBrace :: acc) t
    | '}' -> tokenize_aux (RBrace :: acc) t
    | '.' ->
      (match t with
      | '.' :: t -> tokenize_aux (DoubleDot :: acc) t
      | _ -> tokenize_aux (Dot :: acc) t)
    | '^' -> tokenize_aux (Hat :: acc) t
    | ';' ->
      (match t with
      | ';' :: t -> tokenize_aux (DoubleSemicolon :: acc) t
      | _ -> tokenize_aux (Semicolon :: acc) t)
    | ':' ->
      (match t with
      | ':' :: t -> tokenize_aux (DoubleColon :: acc) t
      | _ -> tokenize_aux (Colon :: acc) t)
    | _ -> failwith @@ Printf.sprintf "unexpected character: '%c'" h)
;;

let string_of_token = function
  | IntLiteral num -> string_of_int num
  | BoolLiteral b -> string_of_bool b
  | StringLiteral str -> Printf.sprintf "\"%s\"" str
  | CharLiteral ch -> Printf.sprintf "'%c'" ch
  | CapitalIdent ident | LowerIdent ident -> ident
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Let -> "let"
  | Rec -> "rec"
  | In -> "in"
  | And -> "and"
  | Equal -> "="
  | DoubleEqual -> "=="
  | NotEqual -> "!="
  | LtGt -> "<>"
  | Lt -> "<"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Type -> "type"
  | Vertical -> "|"
  | Excl -> "!"
  | Of -> "of"
  | Match -> "match"
  | With -> "with"
  | When -> "when"
  | Fun -> "fun"
  | Arrow -> "->"
  | Function -> "function"
  | Comma -> ","
  | Semicolon -> ";"
  | DoubleSemicolon -> ";;"
  | Colon -> ":"
  | DoubleColon -> "::"
  | LParen -> "("
  | RParen -> ")"
  | LBracket -> "["
  | RBracket -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Dot -> "."
  | DoubleDot -> ".."
  | Hat -> "^"
  | Apostrophe -> "'"
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
