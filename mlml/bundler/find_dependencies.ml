module SS = Tree.Simple_set
module TyExpr = Tree.Type_expression
module Expr = Tree.Expression
module Mod = Tree.Module
module Path = Tree.Path
module Pat = Tree.Pattern

type mutable_set = {mutable data : string SS.t}

let empty () = {data = SS.empty}
let union s x = s.data <- SS.union x s.data
let add s x = s.data <- SS.add x s.data

type env = mutable_set

let deps_of_module_path env path =
  let h = Path.head path in
  if SS.mem h env.data then SS.empty else SS.singleton h
;;

let deps_of_path env path =
  match Path.is_single path with
  | true -> SS.empty
  | false -> deps_of_module_path env path
;;

let vars env s path _ = union s @@ deps_of_path env path
let binds x _ = x

let type_expression env ex =
  let s = empty () in
  ignore @@ TyExpr.apply_on_names (vars env s) binds ex;
  s.data
;;

let type_definition env = function
  | Mod.Variant l ->
    let aux = function _, Some x -> type_expression env x | _, None -> SS.empty in
    List.map aux l |> List.fold_left SS.union SS.empty
  | Mod.Record l ->
    let aux (_, _, x) = type_expression env x in
    List.map aux l |> List.fold_left SS.union SS.empty
  | Mod.Alias x -> type_expression env x
;;

let pattern env p =
  let s = empty () in
  ignore @@ Pat.apply_on_names (vars env s) binds p;
  s.data
;;

let expression env e =
  let s = empty () in
  ignore @@ Expr.apply_on_names (vars env s) binds e;
  s.data
;;

let rec definition env = function
  | Mod.LetAnd (_, l) ->
    let aux = function
      | Expr.VarBind (p, b) | Expr.FunBind (_, p, b) ->
        SS.union (pattern env p) (expression env b)
    in
    List.map aux l |> List.fold_left SS.union SS.empty
  | Mod.TypeDef l ->
    let aux (_, _, def) = type_definition env def in
    List.map aux l |> List.fold_left SS.union SS.empty
  | Mod.Module (name, Mod.Path path) ->
    let s = deps_of_module_path env path in
    add env name;
    s
  | Mod.Module (name, Mod.Struct l) ->
    let s = module_items env l in
    add env name;
    s
  | Mod.Open path -> deps_of_module_path env path
  | Mod.External (_, ty, _) -> type_expression env ty

and module_item env = function
  | Mod.Expression expr -> expression env expr
  | Mod.Definition defn -> definition env defn

and module_items env l = List.map (module_item env) l |> List.fold_left SS.union SS.empty

let f l =
  let env = empty () in
  module_items env l
;;
