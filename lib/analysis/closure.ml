module P = Parser
module Expr = P.Expression
module Pat = P.Pattern
module SS = Set.Make (String)

let rec free_variables = function
  | Expr.Int _ -> SS.empty
  | Expr.Add (l, r)
  | Expr.Sub (l, r)
  | Expr.Mul (l, r)
  | Expr.Follow (l, r)
  | Expr.App (l, r)
  | Expr.Equal (l, r) -> SS.union (free_variables l) (free_variables r)
  | Expr.Tuple values ->
    List.map free_variables values |> List.fold_left SS.union SS.empty
  | Expr.LetVar (pat, lhs, rhs) ->
    let intros = Pat.introduced_idents pat in
    let lhs = free_variables lhs in
    let rhs = free_variables rhs in
    SS.union lhs (SS.diff rhs intros)
  | Expr.LetFun (is_rec, ident, param, body, in_) ->
    let intros = SS.singleton ident in
    let param = Pat.introduced_idents param in
    let param = if is_rec then SS.add ident param else param in
    let body = free_variables body in
    let in_ = free_variables in_ in
    SS.union (SS.diff body param) (SS.diff in_ intros)
  | Expr.IfThenElse (c, t, e) ->
    SS.union (free_variables c) @@ SS.union (free_variables t) (free_variables e)
  | Expr.Ctor (_, expr) ->
    (match expr with Some expr -> free_variables expr | None -> SS.empty)
  | Expr.Match (expr, arms) ->
    let expr = free_variables expr in
    let aux (pat, when_, v) =
      let pat_intros = Pat.introduced_idents pat in
      let v = free_variables v in
      match when_ with
      | Some when_ ->
        let when_ = free_variables when_ in
        SS.diff (SS.union when_ v) pat_intros
      | None -> SS.diff v pat_intros
    in
    let arms = List.map aux arms |> List.fold_left SS.union SS.empty in
    SS.union expr arms
  | Expr.Lambda (param, body) ->
    let param = Pat.introduced_idents param in
    let body = free_variables body in
    SS.diff body param
  | Expr.Var x -> SS.singleton x
;;
