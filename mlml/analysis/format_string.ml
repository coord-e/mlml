module Path = Tree.Path
module Expr = Tree.Expression
module Mod = Tree.Module
module Pat = Tree.Pattern
module Fmt = Tree.Format_string
module Binop = Tree.Binop

let fmt_int_function = Path.path_of_string "MlmlInternalFormat.fmt_int"
let fmt_string_function = Path.path_of_string "MlmlInternalFormat.fmt_string"
let fmt_char_function = Path.path_of_string "MlmlInternalFormat.fmt_char"

let convert_format_string fmt =
  let join a_opt b =
    match a_opt with
    | Some a -> Some (Expr.BinOp (Binop.Custom (Path.single "^"), a, b))
    | None -> Some b
  in
  let call_path path b = Expr.App (Expr.Var path, Expr.Var (Path.single b)) in
  let gen = Printf.sprintf "%s%d" in
  (* `f_acc` is a reverse-ordered list of function parameters *)
  (* `s_acc` is a body of format string                       *)
  let rec aux f_acc s_acc = function
    | Fmt.Const s :: rest ->
      let s = Expr.String s in
      aux f_acc (join s_acc s) rest
    | Fmt.Int :: rest ->
      let v = gen "d" (List.length f_acc) in
      let s = call_path fmt_int_function v in
      aux (v :: f_acc) (join s_acc s) rest
    | Fmt.Char :: rest ->
      let v = gen "c" (List.length f_acc) in
      let s = call_path fmt_char_function v in
      aux (v :: f_acc) (join s_acc s) rest
    | Fmt.String :: rest ->
      let v = gen "s" (List.length f_acc) in
      let s = call_path fmt_string_function v in
      aux (v :: f_acc) (join s_acc s) rest
    | [] -> f_acc, s_acc
  in
  let f_acc, s_acc = aux [] None fmt in
  let s_acc = match s_acc with Some s -> s | None -> failwith "empty format string" in
  let s_body = Expr.App (Expr.Var (Path.single "k"), s_acc) in
  let folder acc name = Expr.Lambda (Pat.Var name, acc) in
  let body = List.fold_left folder s_body f_acc in
  Expr.Lambda (Pat.Var "k", body)
;;

let rec convert_let_bindings l =
  let aux = function
    | Expr.VarBind (p, e) -> Expr.VarBind (p, convert_expr e)
    | Expr.FunBind (name, p, e) -> Expr.FunBind (name, p, convert_expr e)
  in
  List.map aux l

and convert_expr e =
  match e with
  | Expr.Format l -> convert_format_string l
  | Expr.LetAnd (is_rec, l, in_) ->
    let l = convert_let_bindings l in
    let in_ = convert_expr in_ in
    Expr.LetAnd (is_rec, l, in_)
  | Expr.Lambda (p, body) ->
    let body = convert_expr body in
    Expr.Lambda (p, body)
  | Expr.Match (expr, l) ->
    let aux (p, when_, arm) =
      let when_ =
        match when_ with Some when_ -> Some (convert_expr when_) | None -> None
      in
      let arm = convert_expr arm in
      p, when_, arm
    in
    let expr = convert_expr expr in
    let l = List.map aux l in
    Expr.Match (expr, l)
  | Expr.String _ | Expr.Var _ | Expr.Nil | Expr.Int _ -> e
  | Expr.Tuple l -> Expr.Tuple (List.map convert_expr l)
  | Expr.Array l -> Expr.Array (List.map convert_expr l)
  | Expr.BinOp (op, l, r) -> Expr.BinOp (op, convert_expr l, convert_expr r)
  | Expr.UnaryOp (op, e) -> Expr.UnaryOp (op, convert_expr e)
  | Expr.IfThenElse (cond, then_, else_) ->
    Expr.IfThenElse (convert_expr cond, convert_expr then_, convert_expr else_)
  | Expr.App (l, r) -> Expr.App (convert_expr l, convert_expr r)
  | Expr.Ctor (_name, None) -> e
  | Expr.Ctor (name, Some param) -> Expr.Ctor (name, Some (convert_expr param))
  | Expr.Record fields ->
    let aux' (name, expr) = name, convert_expr expr in
    Expr.Record (List.map aux' fields)
  | Expr.RecordField (v, field) -> Expr.RecordField (convert_expr v, field)
  | Expr.RecordFieldAssign (v, field, e) ->
    Expr.RecordFieldAssign (convert_expr v, field, convert_expr e)
  | Expr.RecordUpdate (e, fields) ->
    let aux' (name, expr) = name, convert_expr expr in
    Expr.RecordUpdate (convert_expr e, List.map aux' fields)
  | Expr.ArrayAssign (ary, idx, v) ->
    Expr.ArrayAssign (convert_expr ary, convert_expr idx, convert_expr v)
;;

let rec convert_defn defn =
  match defn with
  | Mod.LetAnd (is_rec, l) ->
    let l = convert_let_bindings l in
    Mod.LetAnd (is_rec, l)
  | Mod.TypeDef _ -> defn
  | Mod.Module (_name, Mod.Path _) -> defn
  | Mod.Module (name, Mod.Struct l) ->
    Mod.Module (name, Mod.Struct (convert_module_items l))
  | Mod.External _ | Mod.Open _ -> defn

and convert_module_item = function
  | Mod.Expression expr -> Mod.Expression (convert_expr expr)
  | Mod.Definition defn -> Mod.Definition (convert_defn defn)

and convert_module_items l = List.map convert_module_item l

let f = convert_module_items
