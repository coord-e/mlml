module L = Lexer

type t =
  | Var of string
  | Int of int
  | Tuple of t list
  | Ctor of string * t option
  | Or of t * t

let rec try_parse_pattern_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> tokens, Some (Int num)
  (* TODO: Add boolean value *)
  | L.BoolLiteral b :: tokens -> tokens, Some (Int (if b then 1 else 0))
  | L.LowerIdent ident :: tokens -> tokens, Some (Var ident)
  | L.CapitalIdent ident :: tokens ->
    (match try_parse_pattern_literal tokens with
    | rest, Some p -> rest, Some (Ctor (ident, Some p))
    | _, None -> tokens, Some (Ctor (ident, None)))
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

and parse_pattern_or tokens =
  let tokens, lhs = parse_pattern_literal tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Vertical :: rest ->
      let rest, rhs = parse_pattern_literal rest in
      aux (Or (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_pattern_tuple tokens =
  let rec aux tokens =
    let rest, curr = parse_pattern_or tokens in
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
  | Int x -> string_of_int x
  | Tuple values ->
    List.map string_of_pattern values |> String.concat ", " |> Printf.sprintf "(%s)"
  | Ctor (name, rhs) ->
    (match rhs with
    | Some rhs -> Printf.sprintf "%s (%s)" name (string_of_pattern rhs)
    | None -> name)
  | Or (a, b) -> Printf.sprintf "(%s) | (%s)" (string_of_pattern a) (string_of_pattern b)
;;

let rec introduced_idents = function
  | Var x -> [x]
  | Int _ -> []
  | Tuple values -> List.map introduced_idents values |> List.flatten
  | Ctor (_, value) ->
    (match value with Some value -> introduced_idents value | None -> [])
  | Or (a, b) -> introduced_idents a @ introduced_idents b
;;
