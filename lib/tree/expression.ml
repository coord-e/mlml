module Pat = Pattern

type 'a let_binding =
  | VarBind of 'a Pat.t * 'a t
  | FunBind of string * 'a Pat.t * 'a t

and 'a t =
  | Int of int
  | Tuple of 'a t list
  | String of string
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
