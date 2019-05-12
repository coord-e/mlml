module DepTree = Dependency_tree
module ModCache = Modules_cache
module Build = Build_tree

let filename_to_module path =
  let file = Filename.basename path in
  let name = Filename.chop_suffix file ".ml" in
  String.capitalize_ascii name
;;

(* `prebundle cache path` bundles files under `path` in one module *)
let prebundle cache path =
  let name = Filename.basename path in
  let entry_path = Filename.concat path name ^ ".ml" in
  match Sys.file_exists entry_path with
  | true -> Build.build_tree_root_perm cache entry_path |> DepTree.collapse
  | false ->
    let is_module_file x = Filename.check_suffix x ".ml" in
    let build path =
      let name = filename_to_module path in
      DepTree.Node (path, Build.build_tree_perm cache name false path)
    in
    Sys.readdir path
    |> Array.to_list
    |> List.filter is_module_file
    |> List.map (Filename.concat path)
    |> List.map build
    |> DepTree.collapse_list
;;
