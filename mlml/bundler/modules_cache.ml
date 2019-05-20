type 'a ast = 'a Tree.Module.module_item list
type 'a t = (string, 'a ast) Hashtbl.t

let empty () = Hashtbl.create 32
let add = Hashtbl.add
let find_opt = Hashtbl.find_opt
let find = Hashtbl.find
let copy = Hashtbl.copy

let find_key_opt pred s =
  let f k _v acc = match pred k with true -> Some k | false -> acc in
  Hashtbl.fold f s None
;;

let load_direct file =
  let ic = open_in file in
  let content = really_input_string ic @@ in_channel_length ic in
  close_in ic;
  Lexer.f content |> Parser.Compilation_unit.f
;;

let load_with_opt init_opt s file =
  match find_opt s file with
  | Some tree -> tree
  | None ->
    let tree = load_direct file in
    let tree = match init_opt with Some f -> f tree | None -> tree in
    add s file tree;
    tree
;;

let load_with f = load_with_opt (Some f)
let load = load_with_opt None
let get = find
