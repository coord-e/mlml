module L = Lexer

type t = Var of string

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

and parse_pattern tokens = parse_pattern_literal tokens

let string_of_pattern = function Var x -> x
