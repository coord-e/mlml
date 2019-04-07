(* Parse the compilation unit.                                                   *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/compunit.html#unit-implementation *)

module Item = Module_item

type ast = Item.module_item list
type t = ast

let f tokens =
  let _rest, ast = Item.parse_module_items tokens in
  ast
;;

let string_of_ast = Item.string_of_module_items
