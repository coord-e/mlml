module P = Parser
module Mod = P.Module

let conv_module_item = function
  | Mod.Expression expr -> Mod.Expression (Closure.closure_conversion expr)
  | Mod.Definition defn -> Mod.Definition (Closure.closure_conversion_defn defn)
;;

let f = List.map conv_module_item
