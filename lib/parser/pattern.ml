module L = Lexer
module T = Tree.Pattern

type t = Path.t T.t

let string_of_pattern = T.string_of_pattern Path.string_of_path

let rec parse_fields tokens =
  let continue path expr = function
    | L.Semicolon :: rest ->
      let rest, acc = parse_fields rest in
      rest, (path, expr) :: acc
    | rest -> rest, [path, expr]
  in
  let rest, path = Path.parse_path tokens in
  match rest with
  | L.Equal :: rest ->
    let rest, expr = parse_pattern rest in
<<<<<<< HEAD
    continue path expr rest
  | rest when Path.is_empty path -> rest, []
  | rest -> continue path (Var (Path.last_path path)) rest
||||||| merged common ancestors
    continue name expr rest
  | L.LowerIdent name :: rest -> continue name (Var name) rest
  | rest -> rest, []
=======
    continue name expr rest
  | L.LowerIdent name :: rest -> continue name (T.Var name) rest
  | rest -> rest, []
>>>>>>> develop

and try_parse_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> tokens, Some (T.Int num)
  (* TODO: Add boolean value *)
  | L.BoolLiteral b :: tokens -> tokens, Some (T.Int (if b then 1 else 0))
  (* TODO: Add char value *)
  | L.CharLiteral from :: L.DoubleDot :: L.CharLiteral to_ :: tokens ->
<<<<<<< HEAD
    tokens, Some (Range (from, to_))
  | L.CharLiteral c :: tokens -> tokens, Some (Int (Char.code c))
  | L.StringLiteral s :: tokens -> tokens, Some (String s)
  | L.LowerIdent ident :: tokens -> tokens, Some (Var (Path.single ident))
  | L.CapitalIdent _ :: _ ->
    (match Path.parse_path tokens with
    | rest, Path [] -> rest, None
    | rest, path ->
      (match try_parse_literal rest with
      | rest, Some p -> rest, Some (Ctor (path, Some p))
      | _, None -> rest, Some (Ctor (path, None))))
||||||| merged common ancestors
    tokens, Some (Range (from, to_))
  | L.CharLiteral c :: tokens -> tokens, Some (Int (Char.code c))
  | L.StringLiteral s :: tokens -> tokens, Some (String s)
  | L.LowerIdent ident :: tokens -> tokens, Some (Var ident)
  | L.CapitalIdent ident :: tokens ->
    (match try_parse_literal tokens with
    | rest, Some p -> rest, Some (Ctor (ident, Some p))
    | _, None -> tokens, Some (Ctor (ident, None)))
=======
    tokens, Some (T.Range (from, to_))
  | L.CharLiteral c :: tokens -> tokens, Some (T.Int (Char.code c))
  | L.StringLiteral s :: tokens -> tokens, Some (T.String s)
  | L.LowerIdent "_" :: tokens -> tokens, Some Wildcard
  | L.LowerIdent ident :: tokens -> tokens, Some (T.Var ident)
  | L.CapitalIdent ident :: tokens ->
    (match try_parse_literal tokens with
    | rest, Some p -> rest, Some (T.Ctor (ident, Some p))
    | _, None -> tokens, Some (T.Ctor (ident, None)))
>>>>>>> develop
  | L.LBrace :: rest ->
    let rest, fields = parse_fields rest in
    (match rest with
    | L.RBrace :: rest -> rest, Some (T.Record fields)
    | _ -> failwith "record definition is not terminated")
  | L.LBracket :: rest ->
    let rec aux = function
      | L.RBracket :: rest -> rest, T.Nil
      | L.Semicolon :: rest -> aux rest
      | tokens ->
        let rest, lhs = parse_pattern tokens in
        let rest, rhs = aux rest in
        rest, T.Cons (lhs, rhs)
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
    tokens, T.Cons (lhs, rhs)
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
  | _ -> rest, T.Tuple values

and parse_or tokens =
  let tokens, lhs = parse_tuple tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Vertical :: rest ->
      let rest, rhs = parse_tuple rest in
      aux (T.Or (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_pattern tokens = parse_or tokens
