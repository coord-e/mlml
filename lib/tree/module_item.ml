module Def = Definition
module Expr = Expression

type 'a t =
  | Definition of 'a Def.t
  | Expression of 'a Expr.t

let string_of_module_item f = function
  | Definition def -> Def.string_of_definition f def
  | Expression expr -> Expr.string_of_expression f expr
;;

(* TODO: function composition can make this clearer *)
let string_of_module_items f items =
  List.map (string_of_module_item f) items |> String.concat ";; "
;;
