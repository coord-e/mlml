(* Parse the definition.                                               *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/modules.html#definition *)

module Expr = Expression
module Pat = Pattern
module TyExpr = Type_expression
module L = Lexer

type type_def = Variant of (string * TyExpr.t option) list

type t =
  | LetAnd of bool * Expr.let_binding list
  | TypeDef of string * type_def

let try_parse_type = function
  | L.LowerIdent ident :: L.Equal :: rest ->
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
      | _ -> rest, []
    in
    let rest, ctors = match rest with L.Vertical :: rest | rest -> aux rest in
    rest, Some (TypeDef (ident, Variant ctors))
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
;;

let string_of_definition = function
  | LetAnd (is_rec, l) ->
    let l = List.map Expr.string_of_let_binding l |> String.concat " and " in
    Printf.sprintf "Let %s %s" (if is_rec then "rec" else "") l
  | TypeDef (name, def) -> Printf.sprintf "type %s = %s" name (string_of_type_def def)
;;

let f = parse_definition
