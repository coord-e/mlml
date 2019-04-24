module L = Lexer
module T = Tree.Path

(* parse path from token list *)
let try_parse_path tokens =
  let rec aux = function
    | L.CapitalIdent ident :: L.Dot :: rest ->
      let rest, acc = aux rest in
      rest, ident :: acc
    | L.CapitalIdent ident :: rest | L.LowerIdent ident :: rest -> rest, [ident]
    | rest -> rest, []
  in
  match aux tokens with rest, [] -> rest, None | rest, l -> rest, Some (T.Path l)
;;

let parse_path tokens =
  match try_parse_path tokens with
  | rest, Some path -> rest, path
  | _ -> failwith "path is expected"
;;
