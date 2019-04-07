(* Parse the type expression.                             *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/types.html *)

module L = Lexer

type t =
  | Ident of string
  | Tuple of t list

let rec try_parse_primary = function
  | L.LowerIdent ident :: rest -> rest, Some (Ident ident)
  | L.LParen :: rest ->
    let rest, v = parse_type_expression rest in
    (match rest with L.RParen :: rest -> rest, Some v | _ -> rest, None)
  | tokens -> tokens, None

and parse_primary tokens =
  match try_parse_primary tokens with
  | tokens, Some v -> tokens, v
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"

and parse_tuple tokens =
  let rec aux tokens =
    let rest, curr = parse_primary tokens in
    match rest with
    | L.Star :: rest ->
      let rest, tail = aux rest in
      rest, curr :: tail
    | _ -> rest, [curr]
  in
  let rest, values = aux tokens in
  match values with
  | [] -> failwith "unreachable"
  | [value] -> rest, value
  | _ -> rest, Tuple values

and parse_type_expression tokens = parse_tuple tokens

let rec string_of_type_expression = function
  | Ident ident -> Printf.sprintf "Ident %s" ident
  | Tuple ts ->
    let ts = List.map string_of_type_expression ts |> String.concat " * " in
    Printf.sprintf "Tuple (%s)" ts
;;
