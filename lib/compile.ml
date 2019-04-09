let f source = Lexer.f source |> Parser.Compilation_unit.f |> Analysis.f |> Codegen.f
