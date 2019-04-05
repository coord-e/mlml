open Mlml

let () =
  match Sys.argv with
  | [|_; source|] -> Compile.compile source
  | _ -> failwith "Invalid number of arguments"
;;
