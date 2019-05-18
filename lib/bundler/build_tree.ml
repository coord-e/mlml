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

let is_stdlib = function Stdlib _ -> true | _ -> false

let find_module projs dir name =
  match find_module_opt projs dir name with
  | Some loc -> loc
  | None -> failwith @@ Printf.sprintf "could not find module named %s" name
;;

let rec build_tree cache projs name loc =
  (* run preprocess in first load *)
  ModCache.load_with (preprocess name @@ is_stdlib loc) cache file
  |> Find_deps.f
  |> SS.elements
  |> List.map (build_tree_node cache @@ Filename.dirname file)

and build_tree_node cache projs dir name =
  let loc = find_module cache dir name in
  DepTree.Node (file, build_tree cache projs name loc)
;;

let build_tree_root cache projs file =
  let name = Printf.sprintf "//%s//" file in
  DepTree.Node (file, build_tree cache projs name (Single file))
;;
