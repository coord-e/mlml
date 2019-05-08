type 'a ast = 'a Tree.Module.module_item list
type 'a t = (string, 'a ast) Hashtbl.t

let empty () = Hashtbl.create 32
let add = Hashtbl.add
let find_opt = Hashtbl.find_opt

let load_direct file =
  let ic = open_in file in
  let content = really_input_string ic @@ in_channel_length ic in
  close_in ic;
  Lexer.f content |> Parser.Compilation_unit.f
;;

let load s file =
  match find_opt s file with
  | Some tree -> tree
  | None ->
    let tree = load_direct file in
    add s file tree;
    tree
;;
