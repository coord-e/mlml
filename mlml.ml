Lexer.tokenize "1+12*11" |> Parser.parse |> Parser.ast_to_string |> print_endline
