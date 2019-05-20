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

type module_location =
  | Single of string
  | Submodule of string
  | Stdlib of string

let find_module_opt projs dir name =
  let name = String.uncapitalize_ascii name in
  let std_filename = Printf.sprintf "%s/%s.ml" stdlib_dir name in
  let local_filename = Printf.sprintf "%s/%s.ml" dir name in
  (* find registered dune modules first *)
  let f x = Filename.basename x = name in
  match List.find_opt f projs with
  | Some path -> Some (Submodule path)
  | None ->
    (match Sys.file_exists local_filename, Sys.file_exists std_filename with
    | true, _ when dir <> stdlib_dir -> Some (Single local_filename)
    | _, true -> Some (Stdlib std_filename)
    | _ -> None)
;;

let check_stdlib = function Stdlib _ -> true | _ -> false

let find_module projs dir name =
  match find_module_opt projs dir name with
  | Some loc -> loc
  | None -> failwith @@ Printf.sprintf "could not find module named %s" name
;;

let filename_to_module path =
  let file = Filename.basename path in
  let name = Filename.chop_suffix file ".ml" in
  String.capitalize_ascii name
;;

let rec build_submodule_tree cache projs path =
  let name = Filename.basename path in
  let entry_path = Filename.concat path name ^ ".ml" in
  match Sys.file_exists entry_path with
  | true -> DepTree.Node (entry_path, build_tree cache projs name false entry_path)
  | false ->
    let is_module_file x = Filename.check_suffix x ".ml" in
    let build path =
      let name = filename_to_module path in
      DepTree.Node (path, build_tree cache projs name false path)
    in
    let l =
      Sys.readdir path
      |> Array.to_list
      |> List.filter is_module_file
      |> List.map (Filename.concat path)
      |> List.map build
    in
    DepTree.Submodule (name, l)

and build_tree cache projs name is_stdlib file =
  (* run preprocess in first load *)
  ModCache.load_with (preprocess name is_stdlib) cache file
  |> Find_deps.f
  |> SS.elements
  |> List.map (build_tree_node cache projs @@ Filename.dirname file)

and build_tree_node cache projs dir name =
  let loc = find_module projs dir name in
  match loc with
  | Single file | Stdlib file ->
    DepTree.Node (file, build_tree cache projs name (check_stdlib loc) file)
  | Submodule path -> build_submodule_tree cache projs path
;;

let build_tree_root cache projs file =
  let name = Printf.sprintf "//%s//" file in
  DepTree.Node (file, build_tree cache projs name false file)
;;
