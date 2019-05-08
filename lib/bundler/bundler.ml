module Mod = Tree.Module
module Path = Tree.Path
module ModCache = Modules_cache
module SS = Tree.Simple_set
module DepTree = Dependency_tree

let stdlib_dir =
  match Sys.getenv_opt "MLML_STDLIB_DIR" with Some d -> d | None -> "../../../stdlib"
;;

let stdlibs = ["sys"; "array"; "bytes"; "char"; "string"; "printf"; "list"; "hashtbl"]

let collect_libs dir =
  let read file =
    let ic = open_in @@ Printf.sprintf "%s/%s" dir file in
    let content = really_input_string ic @@ in_channel_length ic in
    close_in ic;
    let name = String.split_on_char '.' file |> List.hd in
    name, content
  in
  Array.map read (Sys.readdir dir) |> Array.to_list
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

let find_module_opt name =
  let name = String.uncapitalize_ascii name in
  let std_filename = Printf.sprintf "%s/%s.ml" stdlib_dir name in
  match Sys.file_exists std_filename with true -> Some (true, std_filename) | _ -> None
;;

let find_module name =
  match find_module_opt name with
  | Some (is_stdlib, file) -> is_stdlib, file
  | None -> failwith @@ Printf.sprintf "could not find module named %s" name
;;

let rec build_tree' cache name is_stdlib file =
  ModCache.load cache file
  |> preprocess name is_stdlib
  |> Find_deps.f
  |> SS.elements
  |> List.map (build_tree_node cache)

and build_tree_node cache name =
  let is_stdlib, file = find_module name in
  DepTree.Node (file, build_tree' cache name is_stdlib file)
;;

let build_tree_root cache file =
  let name = Printf.sprintf "//%s//" file in
  DepTree.Root (build_tree' cache name false file)
;;

let bundle_file cache file =
  let ic = open_in file in
  let content = really_input_string ic @@ in_channel_length ic in
  close_in ic;
  let libs = collect_libs stdlib_dir in
  let p, libs = List.partition (fun (name, _) -> name = "pervasives") libs in
  let p2, libs = List.partition (fun (name, _) -> name = "pervasives2") libs in
  (* TODO: Remove these hacks *)
  let libs = List.map (fun x -> x, List.assoc x libs) stdlibs |> bundle_libs in
  let p = bundle_libs p in
  let p2 = bundle_libs p2 in
  let source =
    Printf.sprintf "%s;;open Pervasives;;%s;;%s;;open Pervasives2;;%s" p libs p2 content
  in
  let tree = Lexer.f source |> Parser.Compilation_unit.f in
  tree
;;

let f file =
  let cache = ModCache.empty () in
  bundle_file cache file
;;
