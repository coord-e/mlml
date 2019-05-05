module Pat = Pattern
module NS = Namespace
module Fmt = Format_string

type 'a let_binding =
  | VarBind of 'a Pat.t * 'a t
  | FunBind of string * 'a Pat.t * 'a t

and 'a t =
  | Int of int
  | Tuple of 'a t list
  | String of string
  | Array of 'a t list
  | Format of Fmt.kind list
  | BinOp of 'a Binop.t * 'a t * 'a t
  | UnaryOp of Unaryop.t * 'a t
  | LetAnd of bool * 'a let_binding list * 'a t
  | IfThenElse of 'a t * 'a t * 'a t
  | App of 'a t * 'a t
  | Ctor of 'a * 'a t option
  | Var of 'a
  | Match of 'a t * ('a Pat.t * 'a t option * 'a t) list
  | Lambda of 'a Pat.t * 'a t
  | Nil
  | Record of ('a * 'a t) list
  | RecordField of 'a t * 'a
  | RecordFieldAssign of 'a t * 'a * 'a t
  | RecordUpdate of 'a t * ('a * 'a t) list
  | ArrayAssign of 'a t * 'a t * 'a t

let is_fun_bind = function FunBind _ -> true | VarBind _ -> false

type ('a, 'b, 'c, 'd) let_binding_internal =
  | InternalVarBind of 'a Pat.t * 'b t
  | InternalFunBind of string * 'c Pat.t * 'd t

(* apply `f` on reference names, apply `g` on binding names *)
let rec apply_on_let_bindings f g is_rec l =
  (* can't use `let_binding` between `intros` and `bodies` *)
  (* because type differs in body and pattern              *)
  (* using `let_binding_internal` instead *)
  let apply = apply_on_names f g in
  let destruct = function
    | VarBind (p, body) -> InternalVarBind (p, body)
    | FunBind (bind, p, body) -> InternalFunBind (bind, p, body)
  and construct = function
    | InternalVarBind (p, body) -> VarBind (p, body)
    | InternalFunBind (bind, p, body) -> FunBind (bind, p, body)
  and intros = function
    | InternalVarBind (p, body) -> InternalVarBind (Pat.apply_on_names f g p, body)
    | InternalFunBind (bind, p, body) ->
      let bind = g bind NS.Var in
      InternalFunBind (bind, p, body)
  and bodies = function
    | InternalFunBind (bind, p, body) ->
      let p = Pat.apply_on_names f g p in
      let body = apply body in
      InternalFunBind (bind, p, body)
    | InternalVarBind (p, body) -> InternalVarBind (p, apply body)
  in
  let l = List.map destruct l in
  let l =
    match is_rec with
    | true -> List.map intros l |> List.map bodies
    | false -> List.map bodies l |> List.map intros
  in
  List.map construct l

(* apply `f` on reference names, apply `g` on binding names *)
and apply_on_names f g e =
  let apply = apply_on_names f g in
  match e with
  | Int i -> Int i
  | String s -> String s
  | Format l -> Format l
  | Nil -> Nil
  | Tuple l -> Tuple (List.map apply l)
  | Array l -> Array (List.map apply l)
  | BinOp (op, l, r) ->
    let l = apply l in
    let r = apply r in
    let op = Binop.apply_on_custom (fun x -> f x NS.Var) op in
    BinOp (op, l, r)
  | UnaryOp (op, e) -> UnaryOp (op, apply e)
  | LetAnd (is_rec, l, in_) ->
    let l = apply_on_let_bindings f g is_rec l in
    let in_ = apply in_ in
    LetAnd (is_rec, l, in_)
  | IfThenElse (c, t, e) ->
    let c = apply c in
    let t = apply t in
    let e = apply e in
    IfThenElse (c, t, e)
  | App (l, r) ->
    let l = apply l in
    let r = apply r in
    App (l, r)
  | Ctor (name, None) -> Ctor (f name NS.Ctor, None)
  | Ctor (name, Some v) ->
    let name = f name NS.Ctor in
    let v = apply v in
    Ctor (name, Some v)
  | Var name -> Var (f name NS.Var)
  | Match (expr, l) ->
    let aux (p, when_, arm) =
      let p = Pat.apply_on_names f g p in
      let when_ = match when_ with Some when_ -> Some (apply when_) | None -> None in
      let arm = apply arm in
      p, when_, arm
    in
    let expr = apply expr in
    let l = List.map aux l in
    Match (expr, l)
  | Lambda (p, expr) ->
    let p = Pat.apply_on_names f g p in
    let expr = apply expr in
    Lambda (p, expr)
  | Record l ->
    let aux (field, expr) = f field NS.Field, apply expr in
    Record (List.map aux l)
  | RecordField (expr, field) ->
    let expr = apply expr in
    let field = f field NS.Field in
    RecordField (expr, field)
  | RecordFieldAssign (record, field, expr) ->
    let record = apply record in
    let expr = apply expr in
    let field = f field NS.Field in
    RecordFieldAssign (record, field, expr)
  | RecordUpdate (expr, l) ->
    let aux (field, expr) = f field NS.Field, apply expr in
    let expr = apply expr in
    let l = List.map aux l in
    RecordUpdate (expr, l)
  | ArrayAssign (ary, idx, v) ->
    let ary = apply ary in
    let idx = apply idx in
    let v = apply v in
    ArrayAssign (ary, idx, v)
;;

let rec string_of_let_binding f = function
  | VarBind (pat, expr) ->
    Printf.sprintf
      "(%s) = (%s)"
      (Pat.string_of_pattern f pat)
      (string_of_expression f expr)
  | FunBind (name, param, expr) ->
    Printf.sprintf
      "%s (%s) = (%s)"
      name
      (Pat.string_of_pattern f param)
      (string_of_expression f expr)

and string_of_expression f = function
  | Int num -> Printf.sprintf "Int %d" num
  | Tuple values ->
    let p = List.map (string_of_expression f) values |> String.concat ", " in
    Printf.sprintf "Tuple (%s)" p
  | Array values ->
    let p = List.map (string_of_expression f) values |> String.concat ", " in
    Printf.sprintf "Array (%s)" p
  | String s -> Printf.sprintf "String \"%s\"" s
  | Format f -> Printf.sprintf "Format \"%s\"" (Fmt.string_of_format_string f)
  | BinOp (op, lhs, rhs) ->
    Printf.sprintf
      "%s (%s) (%s)"
      (Binop.string_of_binop f op)
      (string_of_expression f lhs)
      (string_of_expression f rhs)
  | UnaryOp (op, e) ->
    Printf.sprintf "%s (%s)" (Unaryop.string_of_unaryop op) (string_of_expression f e)
  | App (lhs, rhs) ->
    Printf.sprintf
      "App (%s) (%s)"
      (string_of_expression f lhs)
      (string_of_expression f rhs)
  | LetAnd (is_rec, l, rhs) ->
    let l = List.map (string_of_let_binding f) l |> String.concat " and " in
    Printf.sprintf
      "Let %s %s in (%s)"
      (if is_rec then "rec" else "")
      l
      (string_of_expression f rhs)
  | Ctor (name, rhs) ->
    (match rhs with
    | Some rhs -> Printf.sprintf "Ctor (%s) (%s)" (f name) (string_of_expression f rhs)
    | None -> Printf.sprintf "Ctor (%s)" (f name))
  | IfThenElse (cond, then_, else_) ->
    Printf.sprintf
      "If (%s) then (%s) else (%s)"
      (string_of_expression f cond)
      (string_of_expression f then_)
      (string_of_expression f else_)
  | Var ident -> Printf.sprintf "Var %s" (f ident)
  | Match (expr, arms) ->
    let string_of_when = function
      | Some w -> Printf.sprintf "when (%s)" (string_of_expression f w)
      | None -> ""
    in
    let string_of_arm (pat, when_, arm) =
      Printf.sprintf
        "(%s) %s -> (%s)"
        (Pat.string_of_pattern f pat)
        (string_of_when when_)
        (string_of_expression f arm)
    in
    let p = List.map string_of_arm arms |> String.concat " | " in
    Printf.sprintf "Match (%s) with %s" (string_of_expression f expr) p
  | Lambda (param, body) ->
    let p = Pat.string_of_pattern f param in
    Printf.sprintf "(%s) -> (%s)" p (string_of_expression f body)
  | Nil -> "Nil"
  | Record fields ->
    let aux (name, expr) =
      Printf.sprintf "%s = (%s)" (f name) (string_of_expression f expr)
    in
    List.map aux fields |> String.concat "; " |> Printf.sprintf "{%s}"
  | RecordField (v, field) ->
    Printf.sprintf "RecordField (%s).%s" (string_of_expression f v) (f field)
  | RecordFieldAssign (v, field, e) ->
    Printf.sprintf
      "RecordFieldAssign (%s).%s <- (%s)"
      (string_of_expression f v)
      (f field)
      (string_of_expression f e)
  | RecordUpdate (e, fields) ->
    let aux (name, expr) =
      Printf.sprintf "%s = (%s)" (f name) (string_of_expression f expr)
    in
    List.map aux fields
    |> String.concat "; "
    |> Printf.sprintf "{%s with %s}" (string_of_expression f e)
  | ArrayAssign (ary, idx, v) ->
    Printf.sprintf
      "(%s).(%s) <- (%s)"
      (string_of_expression f ary)
      (string_of_expression f idx)
      (string_of_expression f v)
;;
