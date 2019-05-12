module Mod = Tree.Module
module Path = Tree.Path
module ModCache = Modules_cache
module SS = Tree.Simple_set
module DepTree = Dependency_tree

let stdlib_dir =
  match Sys.getenv_opt "MLML_STDLIB_DIR" with Some d -> d | None -> "../../../stdlib"
;;

let preprocess name is_stdlib tree =
  let form_open name = Mod.Definition (Mod.Open (Path.single name)) in
  let tree =
    match name with
    | "Pervasives" -> tree
    | "Pervasives2" -> form_open "Pervasives" :: tree
    | _ when is_stdlib -> form_open "Pervasives" :: tree
    | _ -> form_open "Pervasives" :: form_open "Pervasives2" :: tree
  in
  let name = String.capitalize_ascii name in
  [Mod.Definition (Mod.Module (name, Mod.Struct tree))]
;;

let find_module_opt dir name =
  let name = String.uncapitalize_ascii name in
  let std_filename = Printf.sprintf "%s/%s.ml" stdlib_dir name in
  let local_filename = Printf.sprintf "%s/%s.ml" dir name in
  match Sys.file_exists local_filename, Sys.file_exists std_filename with
  | true, _ when dir <> stdlib_dir -> Some (false, local_filename)
  | _, true -> Some (true, std_filename)
  | _ -> None
;;

let find_module dir name =
  match find_module_opt dir name with
  | Some (is_stdlib, file) -> is_stdlib, file
  | None -> failwith @@ Printf.sprintf "could not find module named %s" name
;;

let rec build_tree' cache name is_stdlib file =
  (* run preprocess in first load *)
  ModCache.load_with (preprocess name is_stdlib) cache file
  |> Find_deps.f
  |> SS.elements
  |> List.map (build_tree_node cache @@ Filename.dirname file)

and build_tree_node cache dir name =
  let is_stdlib, file = find_module dir name in
  DepTree.Node (file, build_tree' cache name is_stdlib file)
;;

let build_tree_root cache file =
  let name = Printf.sprintf "//%s//" file in
  DepTree.Node (file, build_tree' cache name false file)
;;
