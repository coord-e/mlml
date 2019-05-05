let bundle_file file =
  let ic = open_in file in
  let content = really_input_string ic @@ in_channel_length ic in
  close_in ic;
  let tree = Lexer.f content |> Parser.Compilation_unit.f in
  tree
;;

let f = bundle_file
