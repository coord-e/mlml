module Expr = Expression

type t = Expr.t
type ast = Expr.t

let f tokens =
  let _rest, ast = Expr.f tokens in
  ast
;;

let string_of_ast = Expr.string_of_expression
