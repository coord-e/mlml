open Mlml

let input_line_opt ch = try Some (input_line ch) with End_of_file -> None

let input_all ch =
  let rec aux acc =
    match input_line_opt ch with
    | Some line -> aux (line :: acc)
    | None -> List.rev acc |> String.concat "\n"
  in
  aux []
;;

let open_and_read_result cmd =
  let channel = Unix.open_process_in cmd in
  let result = input_all channel in
  let status = Unix.close_process_in channel in
  (* TODO: Fail if status is not zero or process is signaled *)
  (match status with
  | Unix.WEXITED code ->
    if code != 0
    then Printf.eprintf "Warning: Execution of test code failed with code %d\n" code
  | Unix.WSTOPPED s | Unix.WSIGNALED s ->
    Printf.eprintf "Warning: Execution of test code failed with signal %d\n" s);
  result
;;

let exec_with_mlml_file file =
  let as_file = Filename.temp_file "." ".s" in
  let oc = open_out as_file in
  Printf.fprintf oc "%s\n" @@ Compile.f file;
  close_out oc;
  let exec_file = Filename.temp_file "." "" in
  let ret_code = Sys.command @@ Printf.sprintf "gcc %s -lgc -o %s" as_file exec_file in
  if ret_code != 0 then failwith "Failed to compile resulting assembly";
  open_and_read_result exec_file
;;

let exec_with_mlml source =
  let ml_file = Filename.temp_file "." ".ml" in
  let oc = open_out ml_file in
  Printf.fprintf oc "%s\n" source;
  close_out oc;
  exec_with_mlml_file ml_file
;;

let exec_with_ocaml_file file = open_and_read_result @@ "ocaml " ^ file

let exec_with_ocaml source =
  let ml_file = Filename.temp_file "." ".ml" in
  let oc = open_out ml_file in
  Printf.fprintf oc "%s\n" source;
  close_out oc;
  exec_with_ocaml_file ml_file
;;

let f source =
  let mlml_result = exec_with_mlml source in
  let ocaml_result = exec_with_ocaml source in
  if not (mlml_result = ocaml_result)
  then (
    Printf.eprintf "mlml: (%s)\nocaml: (%s)\n" mlml_result ocaml_result;
    failwith "assertion failed" )
;;

let expr source =
  let source = Printf.sprintf "print_int (%s)" source in
  f source
;;

let bool_expr source =
  let source = Printf.sprintf "if (%s) then 1 else 0" source in
  expr source
;;

let file path expected_result =
  let mlml_result = exec_with_mlml_file path in
  if not (mlml_result = expected_result)
  then (
    Printf.eprintf "mlml: (%s)\nexpected: (%s)\n" mlml_result expected_result;
    failwith "assertion failed" )
;;
