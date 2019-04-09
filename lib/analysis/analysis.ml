module P = Parser
module Item = P.Module_item

let conv_module_item = function
  | Item.Expression expr -> Item.Expression (Closure.closure_conversion expr)
  | Item.Definition defn -> Item.Definition (Closure.closure_conversion_defn defn)
;;

let f = List.map conv_module_item
