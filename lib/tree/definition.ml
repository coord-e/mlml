module Expr = Expression
module Pat = Pattern
module TyExpr = Type_expression

type 'a type_def =
  | Variant of ('a * TyExpr.t option) list
  | Record of ('a * TyExpr.t) list
  | Alias of TyExpr.t

type 'a t =
  | LetAnd of bool * 'a Expr.let_binding list
  | TypeDef of (string list * string * 'a type_def) list

let string_of_type_def f = function
  | Variant variants ->
    let aux (ctor, param) =
      match param with
      | Some p -> Printf.sprintf "%s (%s)" (f ctor) (TyExpr.string_of_type_expression p)
      | None -> ctor
    in
    List.map aux variants |> String.concat " | "
  | Record fields ->
    let aux (name, ty) =
      Printf.sprintf "%s: %s" (f name) (TyExpr.string_of_type_expression ty)
    in
    List.map aux fields |> String.concat "; " |> Printf.sprintf "{%s}"
  | Alias ty -> TyExpr.string_of_type_expression ty
;;

let string_of_definition f = function
  | LetAnd (is_rec, l) ->
    let l = List.map (Expr.string_of_let_binding f) l |> String.concat " and " in
    Printf.sprintf "Let %s %s" (if is_rec then "rec" else "") l
  | TypeDef l ->
    let aux (params, name, def) =
      let params = String.concat ", '" params |> Printf.sprintf "('%s)" in
      Printf.sprintf "%s %s = %s" params name (string_of_type_def f def)
    in
    List.map aux l |> String.concat " and " |> Printf.sprintf "type %s"
;;
