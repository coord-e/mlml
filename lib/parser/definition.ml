(* Parse the definition.                                               *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/modules.html#definition *)

module Expr = Expression
module Pat = Pattern
module TyExpr = Type_expression
module L = Lexer

type type_def =
  | Variant of (string * TyExpr.t option) list
  | Record of (string * TyExpr.t) list

type t =
  | LetAnd of bool * Expr.let_binding list
  | TypeDef of string * type_def

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
  rest, Variant ctors
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
  | L.RBrace :: rest -> rest, Record fields
  | _ -> failwith "record definition is not terminated"
;;

let try_parse_type = function
  | L.LowerIdent ident :: L.Equal :: rest ->
    let rest, def =
      match rest with L.LBrace :: _ -> parse_record rest | _ -> parse_variant rest
    in
    rest, Some (TypeDef (ident, def))
  | tokens -> tokens, None
;;

let try_parse_let tokens =
  match tokens with
  | L.Type :: rest -> try_parse_type rest
  | L.Let :: rest ->
    let rest, is_rec = Expr.parse_rec rest in
    let rest, binds = Expr.parse_let_bindings rest in
    (match rest with
    | L.In :: _ -> tokens, None
    | _ -> rest, Some (LetAnd (is_rec, binds)))
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

let string_of_type_def = function
  | Variant variants ->
    let aux (ctor, param) =
      match param with
      | Some p -> Printf.sprintf "%s (%s)" ctor (TyExpr.string_of_type_expression p)
      | None -> ctor
    in
    List.map aux variants |> String.concat " | "
  | Record fields ->
    let aux (name, ty) =
      Printf.sprintf "%s: %s" name (TyExpr.string_of_type_expression ty)
    in
    List.map aux fields |> String.concat "; " |> Printf.sprintf "{%s}"
;;

let string_of_definition = function
  | LetAnd (is_rec, l) ->
    let l = List.map Expr.string_of_let_binding l |> String.concat " and " in
    Printf.sprintf "Let %s %s" (if is_rec then "rec" else "") l
  | TypeDef (name, def) -> Printf.sprintf "type %s = %s" name (string_of_type_def def)
;;

let f = parse_definition
