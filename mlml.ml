let () = (
  Lexer.tokenize "11+1" |> Parser.parse |> Codegen.codegen |> print_endline
)
