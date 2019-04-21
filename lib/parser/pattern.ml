module L = Lexer

type t =
  | Var of string
  | Int of int
  | String of string
  | Tuple of t list
  | Ctor of string * t option
  | Or of t * t
  | Cons of t * t
  | Nil
  | Record of (string * t) list
  | Range of char * char

let rec parse_fields tokens =
  let continue name expr = function
    | L.Semicolon :: rest ->
      let rest, acc = parse_fields rest in
      rest, (name, expr) :: acc
    | rest -> rest, [name, expr]
  in
  match tokens with
  | L.LowerIdent name :: L.Equal :: rest ->
    let rest, expr = parse_pattern rest in
    continue name expr rest
  | L.LowerIdent name :: rest -> continue name (Var name) rest
  | rest -> rest, []

and try_parse_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> tokens, Some (Int num)
  (* TODO: Add boolean value *)
  | L.BoolLiteral b :: tokens -> tokens, Some (Int (if b then 1 else 0))
  (* TODO: Add char value *)
  | L.CharLiteral from :: L.DoubleDot :: L.CharLiteral to_ :: tokens ->
    tokens, Some (Range (from, to_))
  | L.CharLiteral c :: tokens -> tokens, Some (Int (Char.code c))
  | L.StringLiteral s :: tokens -> tokens, Some (String s)
  | L.LowerIdent ident :: tokens -> tokens, Some (Var ident)
  | L.CapitalIdent ident :: tokens ->
    (match try_parse_literal tokens with
    | rest, Some p -> rest, Some (Ctor (ident, Some p))
    | _, None -> tokens, Some (Ctor (ident, None)))
  | L.LBrace :: rest ->
    let rest, fields = parse_fields rest in
    (match rest with
    | L.RBrace :: rest -> rest, Some (Record fields)
    | _ -> failwith "record definition is not terminated")
  | L.LBracket :: rest ->
    let rec aux = function
      | L.RBracket :: rest -> rest, Nil
      | L.Semicolon :: rest -> aux rest
      | tokens ->
        let rest, lhs = parse_pattern tokens in
        let rest, rhs = aux rest in
        rest, Cons (lhs, rhs)
    in
    let rest, l = aux rest in
    rest, Some l
  | L.LParen :: tokens ->
    let rest, v = parse_pattern tokens in
    (match rest with L.RParen :: rest -> rest, Some v | _ -> rest, None)
  | _ -> tokens, None

and parse_literal tokens =
  match try_parse_literal tokens with
  | tokens, Some v -> tokens, v
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"

and parse_cons tokens =
  let tokens, lhs = parse_literal tokens in
  match tokens with
  | L.DoubleColon :: tokens ->
    let tokens, rhs = parse_cons tokens in
    tokens, Cons (lhs, rhs)
  | _ -> tokens, lhs

and parse_tuple tokens =
  let rec aux tokens =
    let rest, curr = parse_cons tokens in
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

and parse_or tokens =
  let tokens, lhs = parse_tuple tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Vertical :: rest ->
      let rest, rhs = parse_tuple rest in
      aux (Or (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_pattern tokens = parse_or tokens

let rec string_of_pattern = function
  | Var x -> x
  | Int x -> string_of_int x
  | String s -> Printf.sprintf "\"%s\"" s
  | Tuple values ->
    List.map string_of_pattern values |> String.concat ", " |> Printf.sprintf "(%s)"
  | Ctor (name, rhs) ->
    (match rhs with
    | Some rhs -> Printf.sprintf "%s (%s)" name (string_of_pattern rhs)
    | None -> name)
  | Or (a, b) -> Printf.sprintf "(%s) | (%s)" (string_of_pattern a) (string_of_pattern b)
  | Cons (a, b) ->
    Printf.sprintf "(%s) :: (%s)" (string_of_pattern a) (string_of_pattern b)
  | Nil -> "[]"
  | Record fields ->
    let aux (name, expr) = Printf.sprintf "%s = (%s)" name (string_of_pattern expr) in
    List.map aux fields |> String.concat "; " |> Printf.sprintf "{%s}"
  | Range (from, to_) -> Printf.sprintf "'%c' .. '%c'" from to_
;;

module SS = Set.Make (String)

let rec introduced_idents = function
  | Var "_" -> SS.empty
  | Var x -> SS.singleton x
  | Int _ | String _ -> SS.empty
  | Tuple values -> List.map introduced_idents values |> List.fold_left SS.union SS.empty
  | Ctor (_, value) ->
    (match value with Some value -> introduced_idents value | None -> SS.empty)
  | Or (a, b) -> SS.union (introduced_idents a) (introduced_idents b)
  | Cons (a, b) -> SS.union (introduced_idents a) (introduced_idents b)
  | Nil | Range _ -> SS.empty
  | Record fields ->
    let aux (_, p) = introduced_idents p in
    List.map aux fields |> List.fold_left SS.union SS.empty
;;

let introduced_ident_list p = introduced_idents p |> SS.elements
