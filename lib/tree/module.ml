module Expr = Expression
module Pat = Pattern
module TyExpr = Type_expression

type 'a type_def =
  | Variant of (string * 'a TyExpr.t option) list
  | Record of (string * 'a TyExpr.t) list
  | Alias of 'a TyExpr.t

type 'a module_expr =
  | Path of Path.t
  | Struct of 'a module_item list

and 'a definition =
  | LetAnd of bool * 'a Expr.let_binding list
  | TypeDef of (string list * string * 'a type_def) list
  | Module of string * 'a module_expr
  | Open of 'a

and 'a module_item =
  | Definition of 'a definition
  | Expression of 'a Expr.t

let rec string_of_type_def f = function
  | Variant variants ->
    let aux (ctor, param) =
      match param with
      | Some p -> Printf.sprintf "%s (%s)" ctor (TyExpr.string_of_type_expression f p)
      | None -> ctor
    in
    List.map aux variants |> String.concat " | "
  | Record fields ->
    let aux (name, ty) =
      Printf.sprintf "%s: %s" name (TyExpr.string_of_type_expression f ty)
    in
    List.map aux fields |> String.concat "; " |> Printf.sprintf "{%s}"
  | Alias ty -> TyExpr.string_of_type_expression f ty

and string_of_module_expression f = function
  | Path p -> Path.string_of_path p
  | Struct l ->
    List.map (string_of_module_item f) l
    |> String.concat ";; "
    |> Printf.sprintf "struct %s end"

and string_of_definition f = function
  | LetAnd (is_rec, l) ->
    let l = List.map (Expr.string_of_let_binding f) l |> String.concat " and " in
    Printf.sprintf "Let %s %s" (if is_rec then "rec" else "") l
  | TypeDef l ->
    let aux (params, name, def) =
      let params = String.concat ", '" params |> Printf.sprintf "('%s)" in
      Printf.sprintf "%s %s = %s" params name (string_of_type_def f def)
    in
    List.map aux l |> String.concat " and " |> Printf.sprintf "type %s"
  | Module (name, mexp) ->
    Printf.sprintf "module %s = (%s)" name (string_of_module_expression f mexp)
  | Open path -> Printf.sprintf "open %s" (f path)

and string_of_module_item f = function
  | Definition def -> string_of_definition f def
  | Expression expr -> Expr.string_of_expression f expr
;;

(* TODO: function composition can make this clearer *)
let string_of_module_items f items =
  List.map (string_of_module_item f) items |> String.concat ";; "
;;
