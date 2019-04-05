module L = Lexer

type ast =
  | Int of int
  | Add of ast * ast
  | Mul of ast * ast
  | LetVar of string * ast * ast
  | LetFun of string * string list * ast * ast
  | IfThenElse of ast * ast * ast
  | App of ast * ast
  | Var of string

let rec try_parse_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> tokens, Some (Int num)
  (* TODO: Add boolean value *)
  | L.BoolLiteral b :: tokens -> tokens, Some (Int (if b then 1 else 0))
  | L.LowerIdent ident :: tokens -> tokens, Some (Var ident)
  | L.LParen :: tokens ->
    let rest, v = parse_expression tokens in
    (match rest with L.RParen :: rest -> rest, Some v | _ -> rest, None)
  | _ -> tokens, None

and parse_literal tokens =
  match try_parse_literal tokens with
  | tokens, Some v -> tokens, v
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"

and parse_app tokens =
  let rest, f = parse_literal tokens in
  let rec aux lhs tokens =
    match try_parse_literal tokens with
    | rest, Some p -> aux (App (lhs, p)) rest
    | rest, None -> rest, lhs
  in
  aux f rest

and parse_mult tokens =
  let tokens, lhs = parse_app tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Star :: rest ->
      let rest, rhs = parse_app rest in
      aux (Mul (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_add tokens =
  let tokens, lhs = parse_mult tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Plus :: rest ->
      let rest, rhs = parse_mult rest in
      aux (Add (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_if = function
  | L.If :: rest ->
    let rest, cond = parse_expression rest in
    (match rest with
    | L.Then :: rest ->
      let rest, then_ = parse_expression rest in
      (match rest with
      | L.Else :: rest ->
        let rest, else_ = parse_expression rest in
        rest, IfThenElse (cond, then_, else_)
      | _ -> failwith "could not find 'else'")
    | _ -> failwith "could not find 'then'")
  | tokens -> parse_add tokens

and parse_let = function
  | L.Let :: L.LowerIdent ident :: L.Equal :: rest ->
    let rest, lhs = parse_expression rest in
    (match rest with
    | L.In :: rest ->
      let rest, rhs = parse_expression rest in
      rest, LetVar (ident, lhs, rhs)
    | _ -> failwith "could not find 'in'")
  | L.Let :: L.LowerIdent ident :: rest ->
    let rec aux = function
      | L.Equal :: rest -> rest, []
      | L.LowerIdent ident :: rest ->
        let rest, acc = aux rest in
        rest, ident :: acc
      | _ -> failwith "could not find '='"
    in
    let rest, params = aux rest in
    let rest, lhs = parse_expression rest in
    (match rest with
    | L.In :: rest ->
      let rest, rhs = parse_expression rest in
      rest, LetFun (ident, params, lhs, rhs)
    | _ -> failwith "could not find 'in'")
  | tokens -> parse_if tokens

and parse_expression tokens = parse_let tokens

let parse tokens =
  let _rest, ast = parse_expression tokens in
  ast
;;

let rec string_of_ast = function
  | Int num -> Printf.sprintf "Int %d" num
  | Add (lhs, rhs) ->
    Printf.sprintf "Add (%s) (%s)" (string_of_ast lhs) (string_of_ast rhs)
  | Mul (lhs, rhs) ->
    Printf.sprintf "Mul (%s) (%s)" (string_of_ast lhs) (string_of_ast rhs)
  | LetVar (ident, lhs, rhs) ->
    Printf.sprintf
      "Let (%s) = (%s) in (%s)"
      ident
      (string_of_ast lhs)
      (string_of_ast rhs)
  | LetFun (ident, params, lhs, rhs) ->
    let p = String.concat ", " params in
    Printf.sprintf
      "Let (%s) (%s) = (%s) in (%s)"
      ident
      p
      (string_of_ast lhs)
      (string_of_ast rhs)
  | App (lhs, rhs) ->
    Printf.sprintf "App (%s) (%s)" (string_of_ast lhs) (string_of_ast rhs)
  | Var ident -> Printf.sprintf "Var %s" ident
;;
