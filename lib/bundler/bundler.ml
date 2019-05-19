module Mod = Tree.Module
module Path = Tree.Path
module ModCache = Modules_cache
module SS = Tree.Simple_set
module DepTree = Dependency_tree
module Build = Build_tree
module Find = Find_projects

let rec bundle_libs cache libs =
  let f = function
    | DepTree.Entry p -> ModCache.get cache p
    | DepTree.Scoped (name, l) ->
      let name = String.capitalize_ascii name in
      let l = bundle_libs cache l in
      [Mod.Definition (Mod.Module (name, Mod.Struct l))]
  in
  List.rev_map f libs |> List.flatten
;;

let f file =
  let cache = ModCache.empty () in
  let projects = Find.find_project_root file |> Find.find_projects |> SS.elements in
  Build.build_tree_root cache projects file |> DepTree.collapse |> bundle_libs cache
;;
