type token =
  | IntLiteral of int
  | Plus
  | Star

let to_digit c = int_of_char c - int_of_char '0'

let rec read_int acc rest =
  match rest with
  | h :: t -> (
    match h with
    | '0' .. '9' -> read_int (acc * 10 + to_digit h) t
    | _ -> (rest, acc)
  )
  | _ -> ([], acc)

let rec tokenize_aux acc rest =
  match rest with
  | [] -> acc
  | h :: t ->
    match h with
    | '0' .. '9' -> (
      let rest, num = read_int 0 rest in
      tokenize_aux (IntLiteral num :: acc) rest
    )
    | '+' -> tokenize_aux (Plus :: acc) t
    | '*' -> tokenize_aux (Star :: acc) t
    | _ -> failwith @@ Printf.sprintf "unexpected character: '%c'" h

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let tokenize source =
  tokenize_aux [] @@ explode source

let token_to_string = function
  | IntLiteral num -> string_of_int num
  | Plus -> "+"
  | Star -> "*"

let tokens_to_string =
  let aux acc t =
    (token_to_string t) ^ ", " ^ acc
  in List.fold_left aux ""
