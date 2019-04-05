let compile source =
  (* Lexer.tokenize source |> Lexer.tokens_to_string |> print_endline *)
  (* Lexer.tokenize source |> Parser.parse |> Parser.string_of_ast |> print_endline *)
  Lexer.tokenize source |> Parser.parse |> Codegen.codegen |> print_endline
;;
