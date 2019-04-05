let compile source = Lexer.tokenize source |> Parser.parse |> Codegen.codegen
