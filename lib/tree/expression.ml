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
  | Format of Fmt.kind list
  | BinOp of Binop.t * 'a t * 'a t
  | LetAnd of bool * 'a let_binding list * 'a t
  | IfThenElse of 'a t * 'a t * 'a t
  | App of 'a t * 'a t
  | Ctor of 'a * 'a t option
  | Var of 'a
  | Match of 'a t * ('a Pat.t * 'a t option * 'a t) list
  | Lambda of 'a Pat.t * 'a t
  | Nil
  | Record of ('a * 'a t) list
  | RecordField of 'a t * string
  | RecordUpdate of 'a t * ('a * 'a t) list

let is_fun_bind = function FunBind _ -> true | VarBind _ -> false

(* apply `f` on reference names, apply `g` on binding names *)
let rec apply_on_names f g e =
  let apply = apply_on_names f g in
  match e with
  | Int i -> Int i
  | String s -> String s
  | Format l -> Format l
  | Nil -> Nil
  | Tuple l -> Tuple (List.map apply l)
  | BinOp (op, l, r) ->
    let l = apply l in
    let r = apply r in
    let op =
      match op with
      | Binop.Custom sym -> Binop.Custom (f (Path.single sym) NS.Var)
      | _ -> op
    in
    BinOp (op, l, r)
  | LetAnd (is_rec, l, in_) ->
    let aux = function
      | VarBind (p, body) -> VarBind (Pat.apply_on_names f g p, apply body)
      | FunBind (bind, p, body) ->
        (* TODO: Improve control flow *)
        let bind = if is_rec then g bind NS.Var else bind in
        let p = Pat.apply_on_names f g p in
        let body = apply body in
        let bind = if not is_rec then g bind NS.Var else bind in
        FunBind (bind, p, body)
    in
    let l = List.map aux l in
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
      let when_ = match when_ with Some when_ -> Some (apply when_) | None -> None in
      let p = Pat.apply_on_names f g p in
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
  | RecordField (expr, field_name) -> RecordField (apply expr, field_name)
  | RecordUpdate (expr, l) ->
    let aux (field, expr) = f field NS.Field, apply expr in
    let expr = apply expr in
    let l = List.map aux l in
    RecordUpdate (expr, l)
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
  | String s -> Printf.sprintf "String \"%s\"" s
  | Format f -> Printf.sprintf "Format \"%s\"" (Fmt.string_of_format_string f)
  | BinOp (op, lhs, rhs) ->
    Printf.sprintf
      "%s (%s) (%s)"
      (Binop.string_of_binop op)
      (string_of_expression f lhs)
      (string_of_expression f rhs)
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
    Printf.sprintf "RecordField (%s).%s" (string_of_expression f v) field
  | RecordUpdate (e, fields) ->
    let aux (name, expr) =
      Printf.sprintf "%s = (%s)" (f name) (string_of_expression f expr)
    in
    List.map aux fields
    |> String.concat "; "
    |> Printf.sprintf "{%s with %s}" (string_of_expression f e)
;;
