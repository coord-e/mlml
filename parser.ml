module L = Lexer

type ast =
  | Int of int
  | Add of ast * ast
  | Mul of ast * ast
  | LetVar of string * ast * ast
  | Var of string

let parse_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> (tokens, Int num)
  | L.LowerIdent ident :: tokens  -> (tokens, Var ident)
  | h :: _ -> failwith @@ Printf.sprintf "unexpected token: '%s'" (L.token_to_string h)
  | _ -> failwith "Empty input"

let parse_mult tokens =
  let tokens, lhs = parse_literal tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Star :: rest ->
      let rest, rhs = parse_literal rest in
      aux (Mul (lhs, rhs)) rest
    | _ -> (tokens, lhs)
  in aux lhs tokens

let parse_add tokens =
  let tokens, lhs = parse_mult tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Plus :: rest ->
      let rest, rhs = parse_mult rest in
      aux (Add (lhs, rhs)) rest
    | _ -> (tokens, lhs)
  in aux lhs tokens

let rec parse_let = function
  | L.Let :: L.LowerIdent ident :: L.Equal :: rest -> (
    let rest, lhs = parse_expression rest in
    match rest with
    | L.In :: rest -> (
      let rest, rhs = parse_expression rest in
      (rest, LetVar (ident, lhs, rhs))
    )
    | _ -> failwith "could not find 'in'"
  )
  | tokens -> parse_add tokens

and parse_expression tokens = parse_let tokens

let parse tokens =
  let _rest, ast = parse_expression tokens in
  ast

let rec ast_to_string = function
  | Int num -> Printf.sprintf "Int %d" num
  | Add (lhs, rhs) -> Printf.sprintf "Add (%s) (%s)" (ast_to_string lhs) (ast_to_string rhs)
  | Mul (lhs, rhs) -> Printf.sprintf "Mul (%s) (%s)" (ast_to_string lhs) (ast_to_string rhs)
  | LetVar (ident, lhs, rhs) -> Printf.sprintf "Let (%s) (%s) (%s)" ident (ast_to_string lhs) (ast_to_string rhs)
  | Var ident -> Printf.sprintf "Var %s" ident
