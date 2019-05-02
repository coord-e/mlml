let () =
  Tester.f {| Printf.printf "this is number %d" 123 |};
  Tester.f
    {| print_string @@ Printf.sprintf "String: %s\tNumber: %d\tChar: %c" "ramen" 42 'd' |};
  Tester.f
    {|
let make_string = Printf.sprintf "s%d" in
print_string @@ make_string 123
  |};
  Tester.f
    {|
let fmt = Printf.printf "the answer to %s, the %s, and %s is %d\n" in
fmt "life" "universe" "everything" 42;
let ramen_ver = fmt "ramen"  in
ramen_ver "universe" "everything" 42;
let foods = ramen_ver "steak" "sushi" in
foods 42
  |}
;;
