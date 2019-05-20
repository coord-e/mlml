module SS = Tree.Simple_set

let parent path =
  match Filename.check_suffix path "/" with
  | false -> Filename.dirname path
  | true ->
    let path = Filename.chop_suffix path "/" in
    Filename.dirname path
;;

let absolute path =
  match Filename.is_relative path with
  | true ->
    let cwd = Sys.getcwd () in
    Filename.concat cwd path
  | false -> path
;;

let find_project_root_opt dir =
  let rec aux dir =
    let candidate = Filename.concat dir "dune-project" in
    match Sys.file_exists candidate with
    | true -> Some dir
    | false when dir = "/" -> None
    | false -> aux @@ parent dir
  in
  aux @@ absolute dir
;;

let is_library_dune path =
  let ic = open_in path in
  let line = input_line ic in
  close_in ic;
  let target = "(library" in
  let len = min (String.length line) (String.length target) in
  String.sub line 0 len = target
;;

let rec find_projects root_dir =
  let aux acc name =
    let path = Filename.concat root_dir name in
    match Sys.is_directory path with
    | false -> acc
    | true ->
      let acc = SS.union acc @@ find_projects path in
      let dune = Filename.concat path "dune" in
      (match Sys.file_exists dune with
      | true when is_library_dune dune -> SS.add path acc
      | _ -> acc)
  in
  Sys.readdir root_dir |> Array.fold_left aux SS.empty
;;
