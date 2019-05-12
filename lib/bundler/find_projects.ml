module SS = Tree.Simple_set

let parent path =
  match Filename.check_suffix path "/" with
  | false -> Filename.dirname path
  | true ->
    let path = Filename.chop_suffix path "/" in
    Filename.dirname path
;;

let find_project_root dir =
  let rec aux dir =
    let candidate = Filename.concat dir "dune-project" in
    match Sys.file_exists candidate with
    | true -> dir
    | false when dir = "/" -> failwith "could not detect project root"
    | false -> aux @@ parent dir
  in
  if Filename.is_relative dir then failwith "find_project_root with relative dir";
  aux dir
;;

let rec find_projects root_dir =
  let aux acc name =
    let path = Filename.concat root_dir name in
    match Sys.is_directory path with
    | false -> acc
    | true ->
      let acc = SS.union acc @@ find_projects path in
      let dune = Filename.concat path "dune" in
      (match Sys.file_exists dune with true -> SS.add name acc | false -> acc)
  in
  Sys.readdir root_dir |> Array.fold_left aux SS.empty
;;
