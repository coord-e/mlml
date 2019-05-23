(* Parse the ocaml expression.                           *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/expr.html *)

module L = Lexer
module Pat = Pattern
module Binop = Tree.Binop
module Uop = Tree.Unaryop
module T = Tree.Expression

type t = Tree.Path.t T.t

(* fun x y z -> expr                      *)
(* => fun x -> (fun y -> (fun z -> expr)) *)
let rec params_to_lambdas expr = function
  | h :: t -> T.Lambda (h, params_to_lambdas expr t)
  | [] -> expr
;;

let rec parse_fun_params is_let = function
  | L.Arrow :: rest when not is_let -> rest, []
  | L.Equal :: rest when is_let -> rest, []
  | tokens ->
    let rest, pat = Pat.parse_pattern tokens in
    let rest, acc = parse_fun_params is_let rest in
    rest, pat :: acc
;;

let parse_let_fun_params = parse_fun_params true
let parse_lambda_fun_params = parse_fun_params false

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
    let anon_var = "_function_match" in
    ( rest
    , params @ [Tree.Pattern.Var anon_var]
    , T.Match (T.Var (Tree.Path.single anon_var), arms) )
  | rest ->
    let rest, body = parse_expression rest in
    rest, params, body

and parse_let_bindings rest =
  let rec parse_until_in rest =
    let rest, bind = Pat.parse_pattern rest in
    let rest, params = parse_let_fun_params rest in
    let rest, params, lhs = parse_let_fun_body params rest in
    match rest with
    | L.And :: rest ->
      let rest, acc = parse_until_in rest in
      rest, (bind, params, lhs) :: acc
    | rest -> rest, [bind, params, lhs]
  and conv_params (bind, params, lhs) =
    match params with
    | h :: t ->
      (match bind with
      | Tree.Pattern.Var ident -> T.FunBind (ident, h, params_to_lambdas lhs t)
      | _ -> failwith "only variables are allowed to bind functions")
    | [] -> T.VarBind (bind, lhs)
  in
  let rest, acc = parse_until_in rest in
  rest, List.map conv_params acc

and parse_rec = function L.Rec :: rest -> rest, true | tokens -> tokens, false

and parse_in = function
  | L.In :: rest -> parse_expression rest
  | _ -> failwith "could not find `in`"

and parse_fields tokens =
  let continue path expr = function
    | L.Semicolon :: rest ->
      let rest, acc = parse_fields rest in
      rest, (path, expr) :: acc
    | rest -> rest, [path, expr]
  in
  match Path.try_parse_path tokens with
  | L.Equal :: rest, Some path ->
    let rest, expr = parse_tuple rest in
    continue path expr rest
  | rest, None -> rest, []
  | rest, Some path -> continue path (T.Var (Tree.Path.last_path path)) rest

and parse_record tokens =
  let parse_value tokens =
    let rest, fields = parse_fields tokens in
    rest, T.Record fields
  and try_parse_with tokens =
    let rest, expr = parse_tuple tokens in
    match rest with
    | L.With :: rest ->
      let rest, fields = parse_fields rest in
      rest, Some (T.RecordUpdate (expr, fields))
    | _ -> tokens, None
  in
  let aux tokens =
    let rest, v_opt = try_parse_with tokens in
    match v_opt with Some v -> rest, v | None -> parse_value tokens
  in
  let rest, v = match tokens with L.LBrace :: rest | rest -> aux rest in
  match rest with L.RBrace :: rest -> rest, v | _ -> failwith "could not find `}`"

and try_parse_literal tokens =
  match tokens with
  | L.IntLiteral num :: tokens -> tokens, Some (T.Int num)
  (* TODO: Add boolean value *)
  | L.BoolLiteral b :: tokens -> tokens, Some (T.Int (if b then 1 else 0))
  (* TODO: Add char value *)
  | L.CharLiteral c :: tokens -> tokens, Some (T.Int (Char.code c))
  | L.StringLiteral s :: tokens -> tokens, Some (T.String s)
  | L.FormatStringLiteral s :: tokens -> tokens, Some (T.Format s)
  | L.LowerIdent ident :: rest -> rest, Some (T.Var (Tree.Path.single ident))
  | L.CapitalIdent _ :: _ ->
    (match Path.try_parse_path tokens with
    | rest, None -> rest, None
    | rest, Some path when Tree.Path.is_capitalized path ->
      (* Ctor with param should be parsed in parse_ctor_with_param *)
      rest, Some (T.Ctor (path, None))
    | rest, Some path -> rest, Some (T.Var path))
  | L.LBrace :: rest ->
    let rest, r = parse_record rest in
    rest, Some r
  | L.LArray :: rest ->
    let rec aux = function
      | L.RArray :: rest -> rest, []
      | L.Semicolon :: rest -> aux rest
      | tokens ->
        let rest, v = parse_tuple tokens in
        let rest, acc = aux rest in
        rest, v :: acc
    in
    let rest, l = aux rest in
    rest, Some (T.Array l)
  | L.LBracket :: rest ->
    let rec aux = function
      | L.RBracket :: rest -> rest, T.Nil
      | L.Semicolon :: rest -> aux rest
      | tokens ->
        let rest, lhs = parse_tuple tokens in
        let rest, rhs = aux rest in
        rest, T.BinOp (Binop.Cons, lhs, rhs)
    in
    let rest, l = aux rest in
    rest, Some l
  | L.LParen :: L.RParen :: tokens -> tokens, Some (T.Tuple [])
  | L.LParen :: tokens ->
    let rest, v = parse_expression tokens in
    (match rest with L.RParen :: rest -> rest, Some v | _ -> rest, None)
  | _ -> tokens, None

and try_parse_dot tokens =
  let rest, lhs_opt = try_parse_literal tokens in
  let rec aux lhs = function
    | L.Dot :: L.LowerIdent ident :: rest ->
      aux (T.RecordField (lhs, Tree.Path.single ident)) rest
    | L.Dot :: L.LBracket :: rest ->
      let rest, rhs = parse_expression rest in
      (match rest with
      | L.RBracket :: rest -> aux (T.BinOp (Binop.StringIndex, lhs, rhs)) rest
      | _ -> tokens, None)
    | L.Dot :: L.LParen :: rest ->
      let rest, rhs = parse_expression rest in
      (match rest with
      | L.RParen :: rest -> aux (T.BinOp (Binop.ArrayIndex, lhs, rhs)) rest
      | _ -> tokens, None)
    | rest -> rest, Some lhs
  in
  match lhs_opt with Some lhs -> aux lhs rest | None -> rest, None

and parse_dot tokens =
  match try_parse_dot tokens with
  | tokens, Some v -> tokens, v
  | h :: _, None ->
    failwith @@ Printf.sprintf "unexpected token: '%s'" (L.string_of_token h)
  | [], None -> failwith "Empty input"

and parse_app tokens =
  let rest, f = parse_dot tokens in
  let rec aux lhs tokens =
    match try_parse_dot tokens with
    | rest, Some p -> aux (T.App (lhs, p)) rest
    | rest, None -> rest, lhs
  in
  aux f rest

and parse_ctor_with_param tokens =
  match tokens with
  | L.CapitalIdent _ :: _ ->
    (match Path.try_parse_path tokens with
    | rest, Some path when Tree.Path.is_capitalized path ->
      (* Ctor without param should be parsed in parse_literal *)
      (match try_parse_dot rest with
      | rest, Some p -> rest, T.Ctor (path, Some p)
      | _, None -> parse_app tokens)
    | _ -> parse_app tokens)
  | _ -> parse_app tokens

and parse_prefix = function
  | L.Minus :: rest ->
    let rest, expr = parse_prefix rest in
    rest, T.UnaryOp (Uop.Negate, expr)
  | L.Plus :: rest ->
    let rest, expr = parse_prefix rest in
    rest, T.UnaryOp (Uop.Positate, expr)
  | tokens -> parse_ctor_with_param tokens

and parse_mult tokens =
  let tokens, lhs = parse_prefix tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Star :: rest ->
      let rest, rhs = parse_prefix rest in
      aux (T.BinOp (Binop.Mul, lhs, rhs)) rest
    | L.Slash :: rest ->
      let rest, rhs = parse_prefix rest in
      aux (T.BinOp (Binop.Div, lhs, rhs)) rest
    | L.Mod :: rest ->
      let rest, rhs = parse_prefix rest in
      aux (T.BinOp (Binop.Mod, lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_add tokens =
  let tokens, lhs = parse_mult tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Plus :: rest ->
      let rest, rhs = parse_mult rest in
      aux (T.BinOp (Binop.Add, lhs, rhs)) rest
    | L.Minus :: rest ->
      let rest, rhs = parse_mult rest in
      aux (T.BinOp (Binop.Sub, lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_cons tokens =
  let tokens, lhs = parse_add tokens in
  match tokens with
  | L.DoubleColon :: tokens ->
    let tokens, rhs = parse_cons tokens in
    tokens, T.BinOp (Binop.Cons, lhs, rhs)
  | _ -> tokens, lhs

and parse_infix tokens =
  let tokens, lhs = parse_cons tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.InfixSymbol sym :: rest ->
      let rest, rhs = parse_cons rest in
      aux (T.BinOp (Binop.Custom (Tree.Path.single sym), lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_equal tokens =
  let tokens, lhs = parse_infix tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Equal :: rest ->
      let rest, rhs = parse_infix rest in
      aux (T.BinOp (Binop.Equal, lhs, rhs)) rest
    | L.NotEqual :: rest ->
      let rest, rhs = parse_infix rest in
      aux (T.BinOp (Binop.NotPhysicalEqual, lhs, rhs)) rest
    | L.Lt :: rest ->
      let rest, rhs = parse_infix rest in
      aux (T.BinOp (Binop.Lt, lhs, rhs)) rest
    | L.Gt :: rest ->
      let rest, rhs = parse_infix rest in
      aux (T.BinOp (Binop.Gt, lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_and tokens =
  let tokens, lhs = parse_equal tokens in
  match tokens with
  | L.DoubleAnd :: tokens ->
    let tokens, rhs = parse_and tokens in
    tokens, T.BinOp (Binop.And, lhs, rhs)
  | _ -> tokens, lhs

and parse_or tokens =
  let tokens, lhs = parse_and tokens in
  match tokens with
  | L.DoubleVertical :: tokens ->
    let tokens, rhs = parse_or tokens in
    tokens, T.BinOp (Binop.Or, lhs, rhs)
  | _ -> tokens, lhs

and parse_assign tokens =
  let tokens, lhs = parse_or tokens in
  match tokens with
  | L.LeftArrow :: tokens ->
    let tokens, rhs = parse_tuple tokens in
    (match lhs with
    | T.RecordField (e, field) -> tokens, T.RecordFieldAssign (e, field, rhs)
    | T.BinOp (Binop.ArrayIndex, e, idx) -> tokens, T.ArrayAssign (e, idx, rhs)
    | _ -> failwith "lhs of <- must be record field or array index")
  | _ -> tokens, lhs

and parse_if = function
  | L.If :: rest ->
    let rest, cond = parse_expression rest in
    (match rest with
    | L.Then :: rest ->
      let rest, then_ = parse_tuple rest in
      (match rest with
      | L.Else :: rest ->
        let rest, else_ = parse_tuple rest in
        rest, T.IfThenElse (cond, then_, else_)
      | _ ->
        (* (if c then v)             *)
        (* is converted to           *)
        (* (if c then v; () else ()) *)
        let unit_ = T.Tuple [] in
        let then_ = T.BinOp (Binop.Follow, then_, unit_) in
        rest, T.IfThenElse (cond, then_, unit_))
    | _ -> failwith "could not find 'then'")
  | tokens -> parse_assign tokens

and parse_match = function
  | L.Match :: rest ->
    let rest, expr = parse_expression rest in
    (match rest with
    | L.With :: L.Vertical :: rest | L.With :: rest ->
      let rest, arms = parse_match_arm rest in
      rest, T.Match (expr, arms)
    | _ -> failwith "could not find 'with'")
  | tokens -> parse_if tokens

and parse_let = function
  | L.Fun :: rest ->
    let rest, params = parse_lambda_fun_params rest in
    let rest, params, body = parse_let_fun_body params rest in
    rest, params_to_lambdas body params
  | L.Let :: rest ->
    let rest, is_rec = parse_rec rest in
    let rest, binds = parse_let_bindings rest in
    let rest, rhs = parse_in rest in
    rest, T.LetAnd (is_rec, binds, rhs)
  | tokens -> parse_match tokens

and parse_tuple tokens =
  let rec aux = function
    | L.Comma :: rest ->
      let rest, curr = parse_let rest in
      let rest, tail = aux rest in
      rest, curr :: tail
    | tokens -> tokens, []
  in
  let rest, init = parse_let tokens in
  let rest, values = aux rest in
  match values with [] -> rest, init | _ -> rest, T.Tuple (init :: values)

and parse_follow tokens =
  let tokens, lhs = parse_tuple tokens in
  let rec aux lhs tokens =
    match tokens with
    | L.Semicolon :: rest ->
      let rest, rhs = parse_tuple rest in
      aux (T.BinOp (Binop.Follow, lhs, rhs)) rest
    | _ -> tokens, lhs
  in
  aux lhs tokens

and parse_expression tokens = parse_follow tokens

let f = parse_expression
