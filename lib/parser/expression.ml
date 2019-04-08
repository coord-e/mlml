(* Parse the ocaml expression.                           *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)

module L = Lexer
module Pat = Pattern

type t =
  | Int of int
  | Tuple of t list
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Follow of t * t
  | LetVar of Pat.t * t * t
  | LetFun of bool * string * Pat.t list * t * t
  | IfThenElse of t * t * t
  | App of t * t
  | Ctor of string * t option
  | Var of string
  | Equal of t * t
  | NotEqual of t * t
  | PhysicalEqual of t * t
  | NotPhysicalEqual of t * t
  | Match of t * (Pat.t * t option * t) list

let rec parse_let_fun_params = function
  | L.Equal :: rest -> rest, []
  | tokens ->
    let rest, pat = Pat.parse_pattern tokens in
    let rest, acc = parse_let_fun_params rest in
    rest, pat :: acc
;;

let rec parse_match_arm tokens =
  let parse_from_arrow pat when_ = function
    | L.Arrow :: rest ->
      let rest, arm = parse_expression rest in
      (match rest with
      | L.Vertical :: rest ->
        let rest, acc = parse_match_arm rest in
        rest, (pat, when_, arm) :: acc
      | _ -> rest, [pat, when_, arm])
    | _ -> failwith "could not find '->'"
  in
  let rest, pat = Pat.parse_pattern tokens in
  match rest with
  | L.When :: rest ->
    let rest, when_ = parse_expression rest in
    parse_from_arrow pat (Some when_) rest
  | _ -> parse_from_arrow pat None rest

and parse_let_fun_body params = function
  | L.Function :: L.Vertical :: rest | L.Function :: rest ->
    let rest, arms = parse_match_arm rest in
    let anon_var = ".function_match" in
    rest, params @ [Pat.Var anon_var], Match (Var anon_var, arms)
  | rest ->
    let rest, body = parse_expression rest in
    rest, params, body

and try_parse_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> tokens, Some (Int num)
  (* TODO: Add boolean value *)
  | L.BoolLiteral b :: tokens -> tokens, Some (Int (if b then 1 else 0))
  | L.LowerIdent ident :: tokens -> tokens, Some (Var ident)
  | L.CapitalIdent ident :: tokens ->
    (match try_parse_literal tokens with
    | rest, Some p -> rest, Some (Ctor (ident, Some p))
    | _, None -> tokens, Some (Ctor (ident, None)))
  | L.LParen :: tokens ->
    let rest, v = parse_expression tokens in
    (match rest with L.RParen :: rest -> rest, Some v | _ -> rest, None)
  | _ -> tokens, None

and parse_literal tokens =
  match try_parse_literal tokens with
  | tokens, Some v -> tokens, v
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"

and parse_app tokens =
  let rest, f = parse_literal tokens in
  let rec aux lhs tokens =
    match try_parse_literal tokens with
    | rest, Some p -> aux (App (lhs, p)) rest
    | rest, None -> rest, lhs
  in
  aux f rest

and parse_mult tokens =
  let tokens, lhs = parse_app tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Star :: rest ->
      let rest, rhs = parse_app rest in
      aux (Mul (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_add tokens =
  let tokens, lhs = parse_mult tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Plus :: rest ->
      let rest, rhs = parse_mult rest in
      aux (Add (lhs, rhs)) rest
    | L.Minus :: rest ->
      let rest, rhs = parse_mult rest in
      aux (Sub (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_equal tokens =
  let tokens, lhs = parse_add tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Equal :: rest ->
      let rest, rhs = parse_add rest in
      aux (Equal (lhs, rhs)) rest
    | L.DoubleEqual :: rest ->
      let rest, rhs = parse_add rest in
      aux (PhysicalEqual (lhs, rhs)) rest
    | L.LtGt :: rest ->
      let rest, rhs = parse_add rest in
      aux (NotEqual (lhs, rhs)) rest
    | L.NotEqual :: rest ->
      let rest, rhs = parse_add rest in
      aux (NotPhysicalEqual (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_tuple tokens =
  let rec aux tokens =
    let rest, curr = parse_equal tokens in
    match rest with
    | L.Comma :: rest ->
      let rest, tail = aux rest in
      rest, curr :: tail
    | _ -> rest, [curr]
  in
  let rest, values = aux tokens in
  match values with
  | [] -> failwith "unreachable"
  | [value] -> rest, value
  | _ -> rest, Tuple values

and parse_if = function
  | L.If :: rest ->
    let rest, cond = parse_expression rest in
    (match rest with
    | L.Then :: rest ->
      let rest, then_ = parse_expression rest in
      (match rest with
      | L.Else :: rest ->
        let rest, else_ = parse_expression rest in
        rest, IfThenElse (cond, then_, else_)
      | _ -> failwith "could not find 'else'")
    | _ -> failwith "could not find 'then'")
  | tokens -> parse_tuple tokens

and parse_match = function
  | L.Match :: rest ->
    let rest, expr = parse_expression rest in
    (match rest with
    | L.With :: L.Vertical :: rest | L.With :: rest ->
      let rest, arms = parse_match_arm rest in
      rest, Match (expr, arms)
    | _ -> failwith "could not find 'with'")
  | tokens -> parse_if tokens

and parse_let = function
  (* `let rec` -> function definition *)
  | L.Let :: L.Rec :: L.LowerIdent ident :: rest ->
    let rest, params = parse_let_fun_params rest in
    let rest, params, lhs = parse_let_fun_body params rest in
    (match rest with
    | L.In :: rest ->
      let rest, rhs = parse_expression rest in
      (match params with
      (* TODO: Support let rec without arguments *)
      | [] -> failwith "'let rec' without arguments"
      | _ -> rest, LetFun (true, ident, params, lhs, rhs))
    | _ -> failwith "could not find 'in'")
  | L.Let :: L.Rec :: t :: _ ->
    failwith
    @@ Printf.sprintf "unexpected token '%s' after let rec" (L.string_of_token t)
  | L.Let :: rest ->
    let rest, bind = Pat.parse_pattern rest in
    let rest, params, lhs =
      match rest with
      | L.Equal :: L.Function :: _ ->
        (* function *)
        let rest, params = parse_let_fun_params rest in
        parse_let_fun_body params rest
      | L.Equal :: rest ->
        (* variable *)
        let rest, lhs = parse_expression rest in
        rest, [], lhs
      | _ ->
        (* function *)
        let rest, params = parse_let_fun_params rest in
        parse_let_fun_body params rest
    in
    (match rest with
    | L.In :: rest ->
      let rest, rhs = parse_expression rest in
      if List.length params == 0
      then rest, LetVar (bind, lhs, rhs)
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
        rest, LetFun (false, ident, params, lhs, rhs)
    | _ -> failwith "could not find 'in'")
  | tokens -> parse_match tokens

and parse_follow tokens =
  let tokens, lhs = parse_let tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Semicolon :: rest ->
      let rest, rhs = parse_let rest in
      aux (Follow (lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_expression tokens = parse_follow tokens

let rec string_of_expression = function
  | Int num -> Printf.sprintf "Int %d" num
  | Tuple values ->
    let p = List.map string_of_expression values |> String.concat ", " in
    Printf.sprintf "Tuple (%s)" p
  | Add (lhs, rhs) ->
    Printf.sprintf "Add (%s) (%s)" (string_of_expression lhs) (string_of_expression rhs)
  | Sub (lhs, rhs) ->
    Printf.sprintf "Sub (%s) (%s)" (string_of_expression lhs) (string_of_expression rhs)
  | Mul (lhs, rhs) ->
    Printf.sprintf "Mul (%s) (%s)" (string_of_expression lhs) (string_of_expression rhs)
  | Follow (lhs, rhs) ->
    Printf.sprintf "(%s); (%s)" (string_of_expression lhs) (string_of_expression rhs)
  | Equal (lhs, rhs) ->
    Printf.sprintf
      "Equal (%s) (%s)"
      (string_of_expression lhs)
      (string_of_expression rhs)
  | NotEqual (lhs, rhs) ->
    Printf.sprintf
      "NotEqual (%s) (%s)"
      (string_of_expression lhs)
      (string_of_expression rhs)
  | PhysicalEqual (lhs, rhs) ->
    Printf.sprintf
      "PhysicalEqual (%s) (%s)"
      (string_of_expression lhs)
      (string_of_expression rhs)
  | NotPhysicalEqual (lhs, rhs) ->
    Printf.sprintf
      "NotPhysicalEqual (%s) (%s)"
      (string_of_expression lhs)
      (string_of_expression rhs)
  | LetVar (pat, lhs, rhs) ->
    Printf.sprintf
      "Let (%s) = (%s) in (%s)"
      (Pat.string_of_pattern pat)
      (string_of_expression lhs)
      (string_of_expression rhs)
  | LetFun (is_rec, ident, params, lhs, rhs) ->
    let p = List.map Pat.string_of_pattern params |> String.concat ", " in
    Printf.sprintf
      "Let %s (%s) (%s) = (%s) in (%s)"
      (if is_rec then "rec" else "")
      ident
      p
      (string_of_expression lhs)
      (string_of_expression rhs)
  | App (lhs, rhs) ->
    Printf.sprintf "App (%s) (%s)" (string_of_expression lhs) (string_of_expression rhs)
  | Ctor (name, rhs) ->
    (match rhs with
    | Some rhs -> Printf.sprintf "Ctor (%s) (%s)" name (string_of_expression rhs)
    | None -> Printf.sprintf "Ctor (%s)" name)
  | IfThenElse (cond, then_, else_) ->
    Printf.sprintf
      "If (%s) then (%s) else (%s)"
      (string_of_expression cond)
      (string_of_expression then_)
      (string_of_expression else_)
  | Var ident -> Printf.sprintf "Var %s" ident
  | Match (expr, arms) ->
    let string_of_when = function
      | Some w -> Printf.sprintf "when (%s)" (string_of_expression w)
      | None -> ""
    in
    let string_of_arm (pat, when_, arm) =
      Printf.sprintf
        "(%s) %s -> (%s)"
        (Pat.string_of_pattern pat)
        (string_of_when when_)
        (string_of_expression arm)
    in
    let p = List.map string_of_arm arms |> String.concat " | " in
    Printf.sprintf "Match (%s) with %s" (string_of_expression expr) p
;;

let f = parse_expression
