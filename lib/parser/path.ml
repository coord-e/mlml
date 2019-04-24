module L = Lexer
module T = Tree.Path

(* parse path from token list *)
let parse_path tokens =
  let rec aux = function
    | L.CapitalIdent ident :: L.Dot :: rest ->
      let rest, acc = aux rest in
      rest, ident :: acc
    | L.CapitalIdent ident :: rest | L.LowerIdent ident :: rest -> rest, [ident]
    | _ -> failwith "Failed to parse a path"
  in
  let rest, l = aux tokens in
  rest, T.Path l
;;
