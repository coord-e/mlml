let compile source =
  Lexer.tokenize source |> Parser.parse |> Codegen.codegen |> print_endline

let () = (
  match Sys.argv with
  | [| _; source |] -> compile source
  | _ -> failwith "Invalid number of arguments"
)
