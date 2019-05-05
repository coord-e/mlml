open Mlml

let () =
  match Sys.argv with
  | [|_; file|] -> Compile.f file |> print_endline
  | _ -> failwith "Invalid number of arguments"
;;
