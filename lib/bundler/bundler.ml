module Mod = Tree.Module
module Path = Tree.Path
module ModCache = Modules_cache
module SS = Tree.Simple_set
module DepTree = Dependency_tree
module Build = Build_tree

let bundle_libs cache libs = List.rev_map (ModCache.get cache) libs |> List.flatten

let bundle_file cache file =
  build_tree_root cache file |> DepTree.collapse |> bundle_libs cache
;;

let f file =
  let cache = ModCache.empty () in
  bundle_file cache file
;;
