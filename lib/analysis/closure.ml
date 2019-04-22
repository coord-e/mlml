module P = Parser
module Expr = P.Expression
module Mod = P.Module
module Pat = P.Pattern
module Path = P.Path
module SS = Set.Make (Path)

(* TODO: Improve this function's name *)
let rec intros_and_free_of_binding is_rec = function
  | Expr.FunBind (ident, param, body) ->
    let param = Pat.introduced_idents param in
    let param = if is_rec then SS.add ident param else param in
    let body = free_variables body in
    [ident], SS.diff body param
  | Expr.VarBind (pat, body) ->
    let intros = Pat.introduced_ident_list pat in
    let body = free_variables body in
    intros, body

and free_variables = function
  | Expr.Int _ | Expr.String _ | Expr.Nil -> SS.empty
  | Expr.Add (l, r)
  | Expr.Sub (l, r)
  | Expr.Mul (l, r)
  | Expr.Follow (l, r)
  | Expr.App (l, r)
  | Expr.Equal (l, r)
  | Expr.NotEqual (l, r)
  | Expr.PhysicalEqual (l, r)
  | Expr.NotPhysicalEqual (l, r)
  | Expr.StringIndex (l, r)
  | Expr.StringAppend (l, r)
  | Expr.Cons (l, r) -> SS.union (free_variables l) (free_variables r)
  | Expr.Tuple values ->
    List.map free_variables values |> List.fold_left SS.union SS.empty
  | Expr.LetAnd (is_rec, l, in_) ->
    let in_ = free_variables in_ in
    let idents, l = List.map (intros_and_free_of_binding is_rec) l |> List.split in
    let intros = List.flatten idents |> SS.of_list in
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
  | Expr.Record fields ->
    let aux (_, expr) = free_variables expr in
    List.map aux fields |> List.fold_left SS.union SS.empty
  | Expr.RecordField (v, _) -> free_variables v
  | Expr.RecordUpdate (e, fields) ->
    let aux (_, expr) = free_variables expr in
    List.map aux fields |> List.fold_left SS.union (free_variables e)
;;

let free_variable_list x = free_variables x |> SS.elements
let make_let_var bind body in_ = Expr.LetAnd (false, [Expr.VarBind (bind, body)], in_)

let make_let_fun is_rec ident param body in_ =
  Expr.LetAnd (is_rec, [Expr.FunBind (ident, param, body)], in_)
;;

let rec let_bindings_conversion i is_rec fvs l =
  let fv_tuple = Expr.Tuple (List.map (fun x -> Expr.Var x) fvs) in
  let fv_pat = Pat.Tuple (List.map (fun x -> Pat.Var x) fvs) in
  let folder_body_rec acc = function
    | Expr.FunBind (ident, _, _) ->
      make_let_var (Pat.Var ident) (Expr.Tuple [Expr.Var ident; fv_tuple]) acc
    | Expr.VarBind _ -> acc
  in
  let aux = function
    | Expr.FunBind (ident, param, body) ->
      let body = closure_conversion' i body in
      let real_body = if is_rec then List.fold_left folder_body_rec body l else body in
      let real_param = Pat.Tuple [param; fv_pat] in
      let evalto = Expr.Tuple [Expr.Var ident; fv_tuple] in
      (Pat.Var ident, Some evalto), Expr.FunBind (ident, real_param, real_body)
    | Expr.VarBind (pat, body) ->
      let body = closure_conversion' i body in
      (pat, None), Expr.VarBind (pat, body)
  in
  List.map aux l |> List.split

(* TODO: simplify application to subexpr *)
and closure_conversion' i expr =
  (* define an alias because it's a long name *)
  let aux = closure_conversion' in
  match expr with
  | Expr.LetAnd (is_rec, l, in_) ->
    let in_ = aux i in_ in
    let fvs_binding x = intros_and_free_of_binding is_rec x |> snd |> SS.elements in
    let fvs = List.map fvs_binding l |> List.flatten in
    let folder_wrap acc = function
      | pat, Some evalto -> make_let_var pat evalto acc
      | _, None -> acc
    in
    let evals, l = let_bindings_conversion i is_rec fvs l in
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
    let f_name = Path.single @@ Printf.sprintf "_f%d" i in
    let fv_name = Path.single @@ Printf.sprintf "_fv%d" i in
    let destruct = Pat.Tuple [Pat.Var f_name; Pat.Var fv_name] in
    let real_app = Expr.App (Expr.Var f_name, Expr.Tuple [rhs; Expr.Var fv_name]) in
    make_let_var destruct lhs real_app
  (* TODO: Fixup these dirty code *)
  | Expr.Var (Path ["print_int"]) ->
    Expr.Tuple [Expr.Var (Path.single "print_int"); Expr.Tuple []]
  | Expr.Var (Path ["print_char"]) ->
    Expr.Tuple [Expr.Var (Path.single "print_char"); Expr.Tuple []]
  | Expr.Var (Path ["print_string"]) ->
    Expr.Tuple [Expr.Var (Path.single "print_string"); Expr.Tuple []]
  | Expr.Int _ | Expr.Var _ | Expr.String _ | Expr.Nil -> expr
  | Expr.Add (r, l) -> Expr.Add (aux i r, aux i l)
  | Expr.Sub (r, l) -> Expr.Sub (aux i r, aux i l)
  | Expr.Mul (r, l) -> Expr.Mul (aux i r, aux i l)
  | Expr.Follow (r, l) -> Expr.Follow (aux i r, aux i l)
  | Expr.Equal (r, l) -> Expr.Equal (aux i r, aux i l)
  | Expr.NotEqual (r, l) -> Expr.NotEqual (aux i r, aux i l)
  | Expr.PhysicalEqual (r, l) -> Expr.PhysicalEqual (aux i r, aux i l)
  | Expr.NotPhysicalEqual (r, l) -> Expr.NotPhysicalEqual (aux i r, aux i l)
  | Expr.Cons (r, l) -> Expr.Cons (aux i r, aux i l)
  | Expr.StringIndex (r, l) -> Expr.StringIndex (aux i r, aux i l)
  | Expr.StringAppend (r, l) -> Expr.StringAppend (aux i r, aux i l)
  | Expr.IfThenElse (c, t, e) -> Expr.IfThenElse (aux i c, aux i t, aux i e)
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
  | Expr.Record fields ->
    let aux' (name, expr) = name, aux i expr in
    Expr.Record (List.map aux' fields)
  | Expr.RecordField (v, field) -> Expr.RecordField (aux i v, field)
  | Expr.RecordUpdate (e, fields) ->
    let aux' (name, expr) = name, aux i expr in
    Expr.RecordUpdate (aux i e, List.map aux' fields)

and closure_conversion expr = closure_conversion' 0 expr

let make_let_var_defn pat expr = Mod.LetAnd (false, [Expr.VarBind (pat, expr)])

let free_variables_defn = function
  | Mod.LetAnd (is_rec, l) ->
    let _, l = List.map (intros_and_free_of_binding is_rec) l |> List.split in
    List.fold_left SS.union SS.empty l
  | _ -> SS.empty
;;

let rec closure_conversion_defn defn =
  match defn with
  | Mod.LetAnd (is_rec, l) ->
    let fvs = free_variables_defn defn |> SS.elements in
    let evals, l = let_bindings_conversion 0 is_rec fvs l in
    (* Remove VarBind from l, and use body of VarBind in resulting_expr *)
    let funs, vars = List.partition Expr.is_fun_bind l in
    let aux = function
      | Expr.VarBind (pat, body) -> pat, body
      | _ -> failwith "unreachable"
    in
    let folder acc = function
      | pat, Some evalto -> (pat, evalto) :: acc
      | _, None -> acc
    in
    let pats, values = List.fold_left folder (List.map aux vars) evals |> List.split in
    let resulting_pat = Pat.Tuple pats in
    let resulting_expr = Expr.Tuple values in
    make_let_var_defn resulting_pat (Expr.LetAnd (is_rec, funs, resulting_expr))
  | Mod.TypeDef _ -> defn
  | Mod.Module (name, expr) ->
    (match expr with
    | Mod.Path _ -> defn
    | Mod.Struct l ->
      Mod.Module (name, Mod.Struct (List.map closure_conversion_module_item l)))

and closure_conversion_module_item = function
  | Mod.Expression expr -> Mod.Expression (closure_conversion expr)
  | Mod.Definition defn -> Mod.Definition (closure_conversion_defn defn)
;;
