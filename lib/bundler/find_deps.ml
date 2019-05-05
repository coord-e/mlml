module SS = Tree.Simple_set
module TyExpr = Tree.Type_expression
module Expr = Tree.Expression
module Mod = Tree.Module
module Path = Tree.Path
module Pat = Tree.Pattern

let deps_of_path path =
  if Path.is_single path then SS.empty else SS.singleton @@ Path.head path
;;

type mutable_set = {mutable data : string SS.t}

let empty () = {data = SS.empty}
let union s x = s.data <- SS.union x s.data
let vars s path _ = union s @@ deps_of_path path
let binds x _ = x

let type_expression ex =
  let s = empty () in
  let _ = TyExpr.apply_on_names (vars s) binds ex in
  s.data
;;

let type_definition = function
  | Mod.Variant l ->
    let aux = function _, Some x -> type_expression x | _, None -> SS.empty in
    List.map aux l |> List.fold_left SS.union SS.empty
  | Mod.Record l ->
    let aux (_, _, x) = type_expression x in
    List.map aux l |> List.fold_left SS.union SS.empty
  | Mod.Alias x -> type_expression x
;;

let pattern p =
  let s = empty () in
  let _ = Pat.apply_on_names (vars s) binds p in
  s.data
;;

let expression e =
  let s = empty () in
  let _ = Expr.apply_on_names (vars s) binds e in
  s.data
;;

let rec definition = function
  | Mod.LetAnd (_, l) ->
    let aux = function
      | Expr.VarBind (p, b) | Expr.FunBind (_, p, b) ->
        SS.union (pattern p) (expression b)
    in
    List.map aux l |> List.fold_left SS.union SS.empty
  | Mod.TypeDef l ->
    let aux (_, _, def) = type_definition def in
    List.map aux l |> List.fold_left SS.union SS.empty
  | Mod.Module (_, Mod.Path path) -> deps_of_path path
  | Mod.Module (_, Mod.Struct l) -> module_items l
  | Mod.Open path -> deps_of_path path
  | Mod.External (_, ty, _) -> type_expression ty

and module_item = function
  | Mod.Expression expr -> expression expr
  | Mod.Definition defn -> definition defn

and module_items l = List.map module_item l |> List.fold_left SS.union SS.empty

let f = module_items
