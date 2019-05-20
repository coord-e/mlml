let () =
  Tester.f
    {|
let ic = open_in "/etc/issue" in
let content = really_input_string ic @@ in_channel_length ic in
close_in ic;
print_endline content
  |};
  Tester.f
    {|
match Sys.getenv_opt "USER" with
| Some v -> Printf.printf "Some %s\n" v
| None -> print_endline "None"
  |};
  Tester.bool_expr {|
Sys.is_directory "/etc"
  |};
  Tester.bool_expr {|
Sys.is_directory "/etc/issue"
  |};
  Tester.bool_expr {|
Sys.file_exists "/etc"
  |};
  Tester.bool_expr {|
Sys.file_exists "/etc/issue"
  |};
  Tester.expr
    {|
(* TODO: Test against the content *)
Sys.readdir "/etc" |> Array.length
  |}
;;
