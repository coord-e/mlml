module L = Lexer
module T = Tree.Pattern

type t = Tree.Path.t T.t

let string_of_pattern = T.string_of_pattern Tree.Path.string_of_path

let rec parse_fields tokens =
  let continue path expr = function
    | L.Semicolon :: rest ->
      let rest, acc = parse_fields rest in
      rest, (path, expr) :: acc
    | rest -> rest, [path, expr]
  in
  match Path.try_parse_path tokens with
  | L.Equal :: rest, Some path ->
    let rest, expr = parse_pattern rest in
    continue path expr rest
  | rest, None -> rest, []
  | rest, Some path -> continue path (T.Var (Tree.Path.last path)) rest

and try_parse_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> tokens, Some (T.Int num)
  (* TODO: Add boolean value *)
  | L.BoolLiteral b :: tokens -> tokens, Some (T.Int (if b then 1 else 0))
  (* TODO: Add char value *)
  | L.CharLiteral from :: L.DoubleDot :: L.CharLiteral to_ :: tokens ->
    tokens, Some (T.Range (from, to_))
  | L.CharLiteral c :: tokens -> tokens, Some (T.Int (Char.code c))
  | L.StringLiteral s :: tokens -> tokens, Some (T.String s)
  | L.LowerIdent "_" :: tokens -> tokens, Some T.Wildcard
  | L.LowerIdent ident :: tokens -> tokens, Some (T.Var ident)
  | L.LParen :: L.InfixSymbol sym :: L.RParen :: tokens -> tokens, Some (T.Var sym)
  | L.CapitalIdent _ :: _ ->
    (match Path.try_parse_path tokens with
    | rest, None -> rest, None
    | rest, Some path ->
      (match try_parse_literal rest with
      | rest, Some p -> rest, Some (T.Ctor (path, Some p))
      | _, None -> rest, Some (T.Ctor (path, None))))
  | L.LBrace :: rest ->
    let rest, fields = parse_fields rest in
    (match rest with
    | L.RBrace :: rest -> rest, Some (T.Record fields)
    | _ -> failwith "record definition is not terminated")
  | L.LArray :: rest ->
    let rec aux = function
      | L.RArray :: rest -> rest, []
      | L.Semicolon :: rest -> aux rest
      | tokens ->
        let rest, v = parse_pattern tokens in
        let rest, acc = aux rest in
        rest, v :: acc
    in
    let rest, l = aux rest in
    rest, Some (T.Array l)
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
  | L.LParen :: L.RParen :: tokens -> tokens, Some (T.Tuple [])
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
