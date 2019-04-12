open Mlml

let open_and_read_result cmd =
  let channel = Unix.open_process_in cmd in
  let result = input_line channel in
  let status = Unix.close_process_in channel in
  match status with
  | Unix.WEXITED code ->
    if code = 0
    then result
    else failwith @@ Printf.sprintf "Execution of test code failed with code %d" code
  | Unix.WSTOPPED s | Unix.WSIGNALED s ->
    failwith @@ Printf.sprintf "Execution of test code failed with signal %d" s
;;

let exec_with_mlml source =
  let as_file = Filename.temp_file "." ".s" in
  let oc = open_out as_file in
  Printf.fprintf oc "%s\n" @@ Compile.f source;
  close_out oc;
  let exec_file = Filename.temp_file "." "" in
  let ret_code = Sys.command @@ Printf.sprintf "gcc %s -o %s" as_file exec_file in
  if ret_code != 0 then failwith "Failed to compile resulting assembly";
  open_and_read_result exec_file
;;

let exec_with_ocaml source =
  let ml_file = Filename.temp_file "." ".ml" in
  let oc = open_out ml_file in
  Printf.fprintf oc "%s\n" source;
  close_out oc;
  open_and_read_result @@ "ocaml " ^ ml_file
;;

let f source =
  let mlml_result = exec_with_mlml source in
  let ocaml_result = exec_with_ocaml source in
  assert (mlml_result = ocaml_result)
;;

let expr source =
  let source = Printf.sprintf "print_int (%s)" source in
  f source
;;

let bool_expr source =
  let source = Printf.sprintf "if (%s) then 1 else 0" source in
  expr source
;;
