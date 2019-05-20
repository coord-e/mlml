(* Parse the compilation unit.                                                   *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/compunit.html#unit-implementation *)

module Mod = Module

type ast = Mod.module_item list
type t = ast

let f tokens =
  let _rest, ast = Mod.parse_module_items tokens in
  ast
;;

let string_of_ast = Mod.string_of_module_items
