module L = Lexer

type t =
  | Var of string
  | Tuple of t list

let rec try_parse_pattern_literal tokens =
  match tokens with
  | L.LowerIdent ident :: tokens -> tokens, Some (Var ident)
  | L.LParen :: tokens ->
    let rest, v = parse_pattern tokens in
    (match rest with L.RParen :: rest -> rest, Some v | _ -> rest, None)
  | _ -> tokens, None

and parse_pattern_literal tokens =
  match try_parse_pattern_literal tokens with
  | tokens, Some v -> tokens, v
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"

and parse_pattern_tuple tokens =
  let rec aux tokens =
    let rest, curr = parse_pattern_literal tokens in
    match rest with
    | L.Comma :: rest ->
      let rest, tail = aux rest in
      rest, curr :: tail
    | _ -> rest, [curr]
  in
  let rest, values = aux tokens in
  match values with
  | [] -> failwith "unreachable"
  | [value] -> rest, value
  | _ -> rest, Tuple values

and parse_pattern tokens = parse_pattern_tuple tokens

let rec string_of_pattern = function
  | Var x -> x
  | Tuple values ->
    List.map string_of_pattern values |> String.concat ", " |> Printf.sprintf "(%s)"
;;

let rec introduced_idents = function
  | Var x -> [x]
  | Tuple values -> List.map introduced_idents values |> List.flatten
;;