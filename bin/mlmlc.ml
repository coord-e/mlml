open Mlml

let () =
  match Sys.argv with
  | [|_; source|] -> Compile.f source |> print_endline
  | _ -> failwith "Invalid number of arguments"
;;
