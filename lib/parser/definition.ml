(* Parse the definition.                                               *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/modules.html#definition *)

module Expr = Expression
module Pat = Pattern
module TyExpr = Type_expression
module L = Lexer

type t =
  | LetVar of Pat.t * Expr.t
  | LetFun of bool * string * Pat.t list * Expr.t
  | Variant of string * (string * TyExpr.t option) list

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
    rest, Some (Variant (ident, ctors))
  | tokens -> tokens, None
;;

let try_parse_let tokens =
  match tokens with
  | L.Type :: rest -> try_parse_type rest
  (* function definition *)
  | L.Let :: L.Rec :: L.LowerIdent ident :: rest ->
    let rest, params = Expr.parse_let_fun_params rest in
    let rest, params, lhs = Expr.parse_let_fun_body params rest in
    (* check if let-in expression, which is not a definition *)
    (match rest with
    | L.In :: _ -> tokens, None
    | _ -> rest, Some (LetFun (true, ident, params, lhs)))
  | L.Let :: rest ->
    let rest, bind = Pat.parse_pattern rest in
    let rest, params, lhs =
      match rest with
      | L.Equal :: rest ->
        (* variable *)
        let rest, lhs = Expr.parse_expression rest in
        rest, [], lhs
      | _ ->
        (* function *)
        let rest, params = Expr.parse_let_fun_params rest in
        Expr.parse_let_fun_body params rest
    in
    (* check if let-in expression, which is not a definition *)
    (match rest with
    | L.In :: _ -> tokens, None
    | _ ->
      if List.length params == 0
      then rest, Some (LetVar (bind, lhs))
      else
        let ident =
          match bind with
          | Pat.Var x -> x
          | _ ->
            failwith
            @@ Printf.sprintf
                 "cannot name function with pattern '%s'"
                 (Pat.string_of_pattern bind)
        in
        rest, Some (LetFun (false, ident, params, lhs)))
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

let string_of_definition = function
  | LetVar (pat, lhs) ->
    Printf.sprintf
      "Let (%s) = (%s)"
      (Pat.string_of_pattern pat)
      (Expr.string_of_expression lhs)
  | LetFun (is_rec, ident, params, lhs) ->
    let p = List.map Pat.string_of_pattern params |> String.concat ", " in
    Printf.sprintf
      "Let %s (%s) (%s) = (%s)"
      (if is_rec then "rec" else "")
      ident
      p
      (Expr.string_of_expression lhs)
  | Variant (name, variants) ->
    let aux (ctor, param) =
      match param with
      | Some p -> Printf.sprintf "%s (%s)" ctor (TyExpr.string_of_type_expression p)
      | None -> ctor
    in
    let variants = List.map aux variants |> String.concat " | " in
    Printf.sprintf "type %s = %s" name variants
;;

let f = parse_definition
