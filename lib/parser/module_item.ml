(* Parse the module items.                                               *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/modules.html#module-items *)

module T = Tree.Module_item
module Def = Definition
module Expr = Expression
module L = Lexer

type module_item = Path.t T.t

let string_of_module_item = T.string_of_module_item (fun x -> x)
let string_of_module_items = T.string_of_module_items (fun x -> x)

let rec parse_module_items = function
  | L.DoubleSemicolon :: rest -> parse_module_items rest
  | [] -> [], []
  | tokens ->
    let rest, def_opt = Def.try_parse_definition tokens in
    (match def_opt with
    | Some def ->
      let rest, items = parse_module_items rest in
      rest, T.Definition def :: items
    | None ->
      (* may fail in parse_expression (OK because there's no other candidate) *)
      let rest, expr = Expr.parse_expression rest in
      let rest, items = parse_module_items rest in
      rest, T.Expression expr :: items)
;;
