(* Parse the definition.                                               *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/modules.html#definition *)

module Expr = Expression
module Pat = Pattern
module TyExpr = Type_expression
module L = Lexer
module T = Tree.Definition

type t = string T.t

let string_of_definition = T.string_of_definition (fun x -> x)

let parse_variant tokens =
  let rec aux = function
    | L.CapitalIdent name :: L.Of :: rest ->
      let rest, ty_expr = TyExpr.parse_type_expression rest in
      (match rest with
      | L.Vertical :: rest ->
        let rest, acc = aux rest in
        rest, (name, Some ty_expr) :: acc
      | _ -> rest, [name, Some ty_expr])
    | L.CapitalIdent name :: L.Vertical :: rest ->
      let rest, acc = aux rest in
      rest, (name, None) :: acc
    | L.CapitalIdent name :: rest -> rest, [name, None]
    | rest -> rest, []
  in
  let rest, ctors = match tokens with L.Vertical :: rest | rest -> aux rest in
  rest, T.Variant ctors
;;

let parse_record tokens =
  let rec aux = function
    | L.LowerIdent name :: L.Colon :: rest ->
      let rest, ty_expr = TyExpr.parse_type_expression rest in
      (match rest with
      | L.Semicolon :: rest ->
        let rest, acc = aux rest in
        rest, (name, ty_expr) :: acc
      | _ -> rest, [name, ty_expr])
    | rest -> rest, []
  in
  let rest, fields = match tokens with L.LBrace :: rest | rest -> aux rest in
  match rest with
  | L.RBrace :: rest -> rest, T.Record fields
  | _ -> failwith "record definition is not terminated"
;;

let parse_type_params tokens =
  let rec aux = function
    | L.Apostrophe :: L.LowerIdent ident :: L.Comma :: rest ->
      let rest, params = aux rest in
      rest, ident :: params
    | L.Apostrophe :: L.LowerIdent ident :: L.RParen :: rest -> rest, [ident]
    | _ -> failwith "could not parse type params"
  in
  match tokens with
  | L.Apostrophe :: L.LowerIdent ident :: rest -> rest, [ident]
  | L.LParen :: rest -> aux rest
  | _ -> tokens, []
;;

let rec try_parse_type_bindings tokens =
  let rest, params = parse_type_params tokens in
  match rest with
  | L.LowerIdent ident :: L.Equal :: rest ->
    let rest, def =
      match rest with
      | L.LBrace :: _ -> parse_record rest
      | L.Vertical :: _ | L.CapitalIdent _ :: _ -> parse_variant rest
      | _ ->
        let rest, ty = TyExpr.parse_type_expression rest in
        rest, T.Alias ty
    in
    (match rest with
    | L.And :: rest ->
      (match try_parse_type_bindings rest with
      | rest, Some l -> rest, Some ((params, ident, def) :: l)
      | _, None -> failwith "could not parse a binding after `and`")
    | _ -> rest, Some [params, ident, def])
  | tokens -> tokens, None
;;

let try_parse_let tokens =
  match tokens with
  | L.Type :: rest ->
    (match try_parse_type_bindings rest with
    | rest, Some l -> rest, Some (T.TypeDef l)
    | rest, None -> rest, None)
  | L.Let :: rest ->
    let rest, is_rec = Expr.parse_rec rest in
    let rest, binds = Expr.parse_let_bindings rest in
    (match rest with
    | L.In :: _ -> tokens, None
    | _ -> rest, Some (T.LetAnd (is_rec, binds)))
  | tokens -> tokens, None
;;

let try_parse_definition = try_parse_let

let parse_definition tokens =
  match try_parse_definition tokens with
  | rest, Some def -> rest, def
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"
;;

let f = parse_definition
