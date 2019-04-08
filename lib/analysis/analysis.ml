module P = Parser
module Item = P.Module_item

let conv_module_item = function
  | Item.Expression expr -> Item.Expression (Closure.closure_conversion expr)
  | i -> i
;;

let f = List.map conv_module_item
