(* Parse the module items.                                               *)
(* https://caml.inria.fr/pub/docs/manual-ocaml/modules.html#module-items *)

module Def = Definition
module Expr = Expression
module L = Lexer

type module_item =
  | Definition of Def.t
  | Expression of Expr.t

let rec parse_module_items = function
  | L.DoubleSemicolon :: rest -> parse_module_items rest
  | [] -> [], []
  | tokens ->
    let rest, def_opt = Def.try_parse_definition tokens in
    (match def_opt with
    | Some def ->
      let rest, items = parse_module_items rest in
      rest, Definition def :: items
    | None ->
      (* may fail in parse_expression (OK because there's no other candidate) *)
      let rest, expr = Expr.parse_expression rest in
      let rest, items = parse_module_items rest in
      rest, Expression expr :: items)
;;

let string_of_module_item = function
  | Definition def -> Def.string_of_definition def
  | Expression expr -> Expr.string_of_expression expr
;;

(* TODO: function composition can make this clearer *)
let string_of_module_items items =
  List.map string_of_module_item items |> String.concat ";; "
;;
