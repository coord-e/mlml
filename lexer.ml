type token =
  | IntLiteral of int
  | Plus
  | Mult

let to_digit c = int_of_char c - int_of_char '0'

let rec read_int rest acc =
  match rest with
  | h :: t -> (
    match h with
    | '0' .. '9' -> read_int t (acc * 10 + to_digit h)
    | _ -> (acc, rest)
  )
  | _ -> (acc, [])

let rec tokenize_aux rest acc =
  match rest with
  | [] -> acc
  | h :: t ->
    match h with
    | '0' .. '9' -> (
      let num, rest = read_int rest 0 in
      tokenize_aux rest (IntLiteral num :: acc)
    )
    | '+' -> tokenize_aux t (Plus :: acc)
    | '*' -> tokenize_aux t (Mult :: acc)
    | _ -> failwith (Printf.sprintf "unexpected character: '%c'" h)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let tokenize source =
  tokenize_aux (explode source) []

let token_to_string t =
  match t with
  | IntLiteral num -> string_of_int num
  | Plus -> "+"
  | Mult -> "*"

let tokens_to_string l =
  let aux acc t =
    (token_to_string t) ^ ", " ^ acc
  in List.fold_left aux "" l
