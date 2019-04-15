module P = Parser
module Expr = P.Expression
module Def = P.Definition
module Pat = P.Pattern
module SS = Set.Make (String)

let rec free_variables = function
  | Expr.Int _ -> SS.empty
  | Expr.Add (l, r)
  | Expr.Sub (l, r)
  | Expr.Mul (l, r)
  | Expr.Follow (l, r)
  | Expr.App (l, r)
  | Expr.Equal (l, r)
  | Expr.NotEqual (l, r)
  | Expr.PhysicalEqual (l, r)
  | Expr.NotPhysicalEqual (l, r) -> SS.union (free_variables l) (free_variables r)
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
  | Expr.LetAnd (is_rec, l, in_) ->
    let in_ = free_variables in_ in
    let aux (ident, param, body) =
      let param = Pat.introduced_idents param in
      let param = if is_rec then SS.add ident param else param in
      let body = free_variables body in
      ident, SS.diff body param
    in
    let idents, l = List.map aux l |> List.split in
    let intros = SS.of_list idents in
    List.fold_left SS.union (SS.diff in_ intros) l
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
let closure_conversion expr =
  let rec aux i expr =
    match expr with
    | Expr.LetFun (is_rec, ident, param, body, in_) ->
      let fvs = SS.diff (free_variables expr) (free_variables in_) |> SS.elements in
      let body = aux i body in
      let in_ = aux i in_ in
      let fv_tuple = Expr.Tuple (List.map (fun x -> Expr.Var x) fvs) in
      let fv_pat = Pat.Tuple (List.map (fun x -> Pat.Var x) fvs) in
      let real_body =
        if is_rec
        then Expr.LetVar (Pat.Var ident, Expr.Tuple [Expr.Var ident; fv_tuple], body)
        else body
      in
      let real_param = Pat.Tuple [param; fv_pat] in
      let evalto = Expr.Tuple [Expr.Var ident; fv_tuple] in
      let wrap = Expr.LetVar (Pat.Var ident, evalto, in_) in
      Expr.LetFun (is_rec, ident, real_param, real_body, wrap)
    | Expr.LetAnd (is_rec, l, in_) ->
      let in_ = aux i in_ in
      let fvs = SS.diff (free_variables expr) (free_variables in_) |> SS.elements in
      let fv_tuple = Expr.Tuple (List.map (fun x -> Expr.Var x) fvs) in
      let fv_pat = Pat.Tuple (List.map (fun x -> Pat.Var x) fvs) in
      let folder_body_rec acc (ident, _, _) =
        Expr.LetVar (Pat.Var ident, Expr.Tuple [Expr.Var ident; fv_tuple], acc)
      in
      let aux (ident, param, body) =
        let body = aux i body in
        let real_body = if is_rec then List.fold_left folder_body_rec body l else body in
        let real_param = Pat.Tuple [param; fv_pat] in
        let evalto = Expr.Tuple [Expr.Var ident; fv_tuple] in
        (ident, evalto), (ident, real_param, real_body)
      in
      let folder_wrap acc (ident, evalto) = Expr.LetVar (Pat.Var ident, evalto, acc) in
      let evals, l = List.map aux l |> List.split in
      let wrap = List.fold_left folder_wrap in_ evals in
      Expr.LetAnd (is_rec, l, wrap)
    | Expr.Lambda (param, body) ->
      let fvs = free_variable_list expr in
      let body = aux i body in
      let fv_tuple = Expr.Tuple (List.map (fun x -> Expr.Var x) fvs) in
      let fv_pat = Pat.Tuple (List.map (fun x -> Pat.Var x) fvs) in
      let real_param = Pat.Tuple [param; fv_pat] in
      let real_fun = Expr.Lambda (real_param, body) in
      Expr.Tuple [real_fun; fv_tuple]
    | Expr.App (lhs, rhs) ->
      let lhs = aux i lhs in
      let rhs = aux (i + 1) rhs in
      let f_name = Printf.sprintf "_f%d" i in
      let fv_name = Printf.sprintf "_fv%d" i in
      let destruct = Pat.Tuple [Pat.Var f_name; Pat.Var fv_name] in
      let real_app = Expr.App (Expr.Var f_name, Expr.Tuple [rhs; Expr.Var fv_name]) in
      Expr.LetVar (destruct, lhs, real_app)
    | Expr.Var "print_int" -> Expr.Tuple [Expr.Var "print_int"; Expr.Tuple []]
    | Expr.Int _ | Expr.Var _ -> expr
    | Expr.Add (r, l) -> Expr.Add (aux i r, aux i l)
    | Expr.Sub (r, l) -> Expr.Sub (aux i r, aux i l)
    | Expr.Mul (r, l) -> Expr.Mul (aux i r, aux i l)
    | Expr.Follow (r, l) -> Expr.Follow (aux i r, aux i l)
    | Expr.Equal (r, l) -> Expr.Equal (aux i r, aux i l)
    | Expr.NotEqual (r, l) -> Expr.NotEqual (aux i r, aux i l)
    | Expr.PhysicalEqual (r, l) -> Expr.PhysicalEqual (aux i r, aux i l)
    | Expr.NotPhysicalEqual (r, l) -> Expr.NotPhysicalEqual (aux i r, aux i l)
    | Expr.IfThenElse (c, t, e) -> Expr.IfThenElse (aux i c, aux i t, aux i e)
    | Expr.LetVar (pat, lhs, rhs) -> Expr.LetVar (pat, aux i lhs, aux i rhs)
    | Expr.Ctor (name, param) ->
      (match param with
      | Some param -> Expr.Ctor (name, Some (aux i param))
      | None -> expr)
    | Expr.Tuple values -> Expr.Tuple (List.map (aux i) values)
    | Expr.Match (expr, arms) ->
      let expr = aux i expr in
      let aux' (pat, when_, v) =
        let when_ = match when_ with Some when_ -> Some (aux i when_) | None -> None in
        pat, when_, aux i v
      in
      Expr.Match (expr, List.map aux' arms)
  in
  aux 0 expr
;;

let free_variables_defn = function
  | Def.LetFun (is_rec, ident, param, body) ->
    let param = Pat.introduced_idents param in
    let param = if is_rec then SS.add ident param else param in
    let body = free_variables body in
    SS.diff body param
  | Def.LetVar (_pat, lhs) -> free_variables lhs
  | _ -> SS.empty
;;

let closure_conversion_defn defn =
  match defn with
  | Def.LetFun (is_rec, ident, param, body) ->
    let fvs = free_variables_defn defn |> SS.elements in
    let body = closure_conversion body in
    let fv_tuple = Expr.Tuple (List.map (fun x -> Expr.Var x) fvs) in
    let fv_pat = Pat.Tuple (List.map (fun x -> Pat.Var x) fvs) in
    let real_body =
      if is_rec
      then Expr.LetVar (Pat.Var ident, Expr.Tuple [Expr.Var ident; fv_tuple], body)
      else body
    in
    let real_param = Pat.Tuple [param; fv_pat] in
    let evalto = Expr.Tuple [Expr.Var ident; fv_tuple] in
    let f = Expr.LetFun (is_rec, ident, real_param, real_body, evalto) in
    Def.LetVar (Pat.Var ident, f)
  | Def.LetVar (pat, expr) -> Def.LetVar (pat, closure_conversion expr)
  | Def.Variant _ -> defn
;;
