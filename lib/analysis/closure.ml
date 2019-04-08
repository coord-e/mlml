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

let free_variable_list x = free_variables x |> SS.elements

(* TODO: simplify application to subexpr *)
let rec closure_conversion expr =
  match expr with
  | Expr.LetFun (is_rec, ident, param, body, in_) ->
    let fvs = free_variable_list expr in
    let body = closure_conversion body in
    let in_ = closure_conversion in_ in
    let fv_tuple = Expr.Tuple (List.map (fun x -> Expr.Var x) fvs) in
    let fv_pat = Pat.Tuple (List.map (fun x -> Pat.Var x) fvs) in
    let real_param = Pat.Tuple [param; fv_pat] in
    let evalto = Expr.Tuple [Expr.Var ident; fv_tuple] in
    let wrap = Expr.LetVar (Pat.Var ident, evalto, in_) in
    Expr.LetFun (is_rec, ident, real_param, body, wrap)
  | Expr.Lambda (param, body) ->
    let fvs = free_variable_list expr in
    let body = closure_conversion body in
    let fv_tuple = Expr.Tuple (List.map (fun x -> Expr.Var x) fvs) in
    let fv_pat = Pat.Tuple (List.map (fun x -> Pat.Var x) fvs) in
    let real_param = Pat.Tuple [param; fv_pat] in
    let real_fun = Expr.Lambda (real_param, body) in
    Expr.Tuple [real_fun; fv_tuple]
  | Expr.App (Expr.Var "print_int", rhs) ->
    Expr.App (Expr.Var "print_int", closure_conversion rhs)
  | Expr.App (lhs, rhs) ->
    let lhs = closure_conversion lhs in
    let rhs = closure_conversion rhs in
    let destruct = Pat.Tuple [Pat.Var "_f"; Pat.Var "_fv"] in
    let real_app = Expr.App (Expr.Var "_f", Expr.Tuple [rhs; Expr.Var "_fv"]) in
    Expr.LetVar (destruct, lhs, real_app)
  | Expr.Int _ | Expr.Var _ -> expr
  | Expr.Add (r, l) -> Expr.Add (closure_conversion r, closure_conversion l)
  | Expr.Sub (r, l) -> Expr.Sub (closure_conversion r, closure_conversion l)
  | Expr.Mul (r, l) -> Expr.Mul (closure_conversion r, closure_conversion l)
  | Expr.Follow (r, l) -> Expr.Follow (closure_conversion r, closure_conversion l)
  | Expr.Equal (r, l) -> Expr.Equal (closure_conversion r, closure_conversion l)
  | Expr.IfThenElse (c, t, e) -> Expr.IfThenElse (closure_conversion c, closure_conversion t, closure_conversion e)
  | Expr.LetVar (pat, lhs, rhs) -> Expr.LetVar (pat, closure_conversion lhs, closure_conversion rhs)
  | Expr.Ctor (name, param) -> (match param with
    | Some param -> Expr.Ctor (name, Some (closure_conversion param))
    | None -> expr
  )
  | Expr.Tuple values -> Expr.Tuple (List.map closure_conversion values)
  | Expr.Match (expr, arms) ->
      let expr = closure_conversion expr in
      let aux (pat, when_, v) =
        let when_ = (match when_ with
          | Some when_ -> Some (closure_conversion when_)
          | None -> None
        ) in (pat, when_, closure_conversion v)
      in Expr.Match (expr, (List.map aux arms))
;;
