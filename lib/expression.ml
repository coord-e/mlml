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
  | LetVar of Pat.t * t * t
  | LetFun of bool * string * Pat.t list * t * t
  | IfThenElse of t * t * t
  | App of t * t
  | Var of string
  | Equal of t * t

let rec try_parse_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> tokens, Some (Int num)
  (* TODO: Add boolean value *)
  | L.BoolLiteral b :: tokens -> tokens, Some (Int (if b then 1 else 0))
  | L.LowerIdent ident :: tokens -> tokens, Some (Var ident)
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

and parse_let = function
  (* `let rec` -> function definition *)
  | L.Let :: L.Rec :: L.LowerIdent ident :: rest ->
    let rec aux = function
      | L.Equal :: rest -> rest, []
      | tokens ->
        let rest, pat = Pat.parse_pattern tokens in
        let rest, acc = aux rest in
        rest, pat :: acc
    in
    let rest, params = aux rest in
    let rest, lhs = parse_expression rest in
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
      | L.Equal :: rest ->
        (* variable *)
        let rest, lhs = parse_expression rest in
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
        let rest, lhs = parse_expression rest in
        rest, params, lhs
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
  | tokens -> parse_if tokens

and parse_expression tokens = parse_let tokens

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
  | Equal (lhs, rhs) ->
    Printf.sprintf
      "Equal (%s) (%s)"
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
  | IfThenElse (cond, then_, else_) ->
    Printf.sprintf
      "If (%s) then (%s) else (%s)"
      (string_of_expression cond)
      (string_of_expression then_)
      (string_of_expression else_)
  | Var ident -> Printf.sprintf "Var %s" ident
;;

let f = parse_expression
