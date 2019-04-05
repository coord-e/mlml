open Mlml

let () =
  match Sys.argv with
  | [|_; source|] -> Compile.compile source |> print_endline
  | _ -> failwith "Invalid number of arguments"
;;
