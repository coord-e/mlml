(* Parse the definition.                                               *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/modules.html#definition *)

module Expr = Expression
module Pat = Pattern
module L = Lexer

type t =
  | LetVar of Pat.t * Expr.t
  | LetFun of bool * string * Pat.t list * Expr.t

let rec parse_let = function
  (* function definition *)
  | L.Let :: L.Rec :: L.LowerIdent ident :: rest ->
    let rec aux = function
      | L.Equal :: rest -> rest, []
      | tokens ->
        let rest, pat = Pat.parse_pattern tokens in
        let rest, acc = aux rest in
        rest, pat :: acc
    in
    let rest, params = aux rest in
    let rest, lhs = Expr.parse_expression rest in
    rest, LetFun (true, ident, params, lhs)
  | L.Let :: L.Rec :: t :: _ ->
    failwith
    @@ Printf.sprintf "unexpected token '%s' after let rec" (L.string_of_token t)
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
        let rec aux = function
          | L.Equal :: rest -> rest, []
          | tokens ->
            let rest, pat = Pat.parse_pattern tokens in
            let rest, acc = aux rest in
            rest, pat :: acc
        in
        let rest, params = aux rest in
        let rest, lhs = Expr.parse_expression rest in
        rest, params, lhs
    in
    rest, LetFun (false, ident, params, lhs)
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"
;;

let parse_definition = parse_let

let string_of_definition = function
  | LetVar (pat, lhs, rhs) ->
    Printf.sprintf
      "Let (%s) = (%s) in (%s)"
      (Pat.string_of_pattern pat)
      (Expr.string_of_expression lhs)
      (Expr.string_of_expression rhs)
  | LetFun (is_rec, ident, params, lhs, rhs) ->
    let p = List.map Pat.string_of_pattern params |> String.concat ", " in
    Printf.sprintf
      "Let %s (%s) (%s) = (%s) in (%s)"
      (if is_rec then "rec" else "")
      ident
      p
      (Expr.string_of_expression lhs)
      (Expr.string_of_expression rhs)
;;

let f = parse_definition
