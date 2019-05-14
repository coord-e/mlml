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
  |};
  Tester.f
    {|
let f s = Printf.ksprintf (fun x -> s ^ x) in
print_string @@ f "hello, " "%s!" "world";
print_string @@ f "one" "plus %s is %d" "one" 2;
let f2 = f "No." in
f2 "%d-%s" 1 "maybe"
  |};
  Tester.f {|
Printf.printf "Escape%%%s!" "escaped!"
  |}
;;
