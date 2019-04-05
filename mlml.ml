let () = (
  Lexer.tokenize "11+14*11" |> Parser.parse |> Codegen.codegen |> print_endline
)
