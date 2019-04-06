let f source = Lexer.f source |> Parser.f |> Codegen.f
