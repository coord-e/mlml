let () =
  match Sys.argv with
  | [|_; file|] -> Mlml.Compile.f file |> print_endline
  | _ -> failwith "Invalid number of arguments"
;;
