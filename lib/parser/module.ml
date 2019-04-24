(* Parse the definition.                                               *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/modules.html#definition *)

module Expr = Expression
module Pat = Pattern
module TyExpr = Type_expression
module L = Lexer
module T = Tree.Definition

type t = Path.t T.t

let string_of_definition = T.string_of_definition Path.string_of_path

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

let rec try_parse_let tokens =
  match tokens with
  | L.Type :: rest ->
    (match try_parse_type_bindings rest with
    | rest, Some l -> rest, Some (T.TypeDef l)
    | rest, None -> rest, None)
  | L.Module :: L.CapitalIdent ident :: L.Equal :: rest ->
    let rest, expr = parse_module_expression rest in
    rest, Some (Module (ident, expr))
  | L.Let :: rest ->
    let rest, is_rec = Expr.parse_rec rest in
    let rest, binds = Expr.parse_let_bindings rest in
    (match rest with
    | L.In :: _ -> tokens, None
    | _ -> rest, Some (T.LetAnd (is_rec, binds)))
  | tokens -> tokens, None

and try_parse_definition x = try_parse_let x

and parse_definition tokens =
  match try_parse_definition tokens with
  | rest, Some def -> rest, def
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"

and parse_module_items = function
  | L.DoubleSemicolon :: rest -> parse_module_items rest
  | [] -> [], []
  | L.End :: rest -> rest, []
  | tokens ->
    let rest, def_opt = try_parse_definition tokens in
    (match def_opt with
    | Some def ->
      let rest, items = parse_module_items rest in
      rest, Definition def :: items
    | None ->
      (* may fail in parse_expression (OK because there's no other candidate) *)
      let rest, expr = Expr.parse_expression rest in
      let rest, items = parse_module_items rest in
      rest, Expression expr :: items)

and parse_module_expression = function
  | L.Struct :: rest ->
    let rest, l = parse_module_items rest in
    rest, Struct l
  | tokens ->
    let rest, path = Path.parse_path tokens in
    rest, Path path
;;

<<<<<<< HEAD:lib/parser/module.ml
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
  | Alias ty -> TyExpr.string_of_type_expression ty
;;

let rec string_of_definition = function
  | LetAnd (is_rec, l) ->
    let l = List.map Expr.string_of_let_binding l |> String.concat " and " in
    Printf.sprintf "Let %s %s" (if is_rec then "rec" else "") l
  | TypeDef l ->
    let aux (params, name, def) =
      let params = String.concat ", '" params |> Printf.sprintf "('%s)" in
      Printf.sprintf "%s %s = %s" params name (string_of_type_def def)
    in
    List.map aux l |> String.concat " and " |> Printf.sprintf "type %s"
  | Module (name, mexp) ->
    Printf.sprintf "module %s = (%s)" name (string_of_module_expression mexp)

and string_of_module_expression = function
  | Path p -> Path.string_of_path p
  | Struct l ->
    List.map string_of_module_item l
    |> String.concat ";; "
    |> Printf.sprintf "struct %s end"

and string_of_module_item = function
  | Definition def -> string_of_definition def
  | Expression expr -> Expr.string_of_expression expr

(* TODO: function composition can make this clearer *)
and string_of_module_items items =
  List.map string_of_module_item items |> String.concat ";; "
;;

let f = parse_definition
