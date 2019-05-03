let () =
  (* Char *)
  Tester.f
    {|
let test c =
  print_char @@ Char.uppercase_ascii c;
  print_char @@ Char.lowercase_ascii c;
  print_string @@ Char.escaped c;
  print_int @@ Char.code c
;;

test 'a';
test 'x';
test 'C';
test 'Y';
test '\n';
test '\t'
  |};
  (* Hashtbl *)
  Tester.f
    {|
let t = Hashtbl.create 32 in
Hashtbl.add t "hello" "world";
Hashtbl.add t "fantastic" "ramen";
print_string @@ Hashtbl.find t "hello";
print_string @@ Hashtbl.find t "fantastic"
  |};
  Tester.f
    {|
let t = Hashtbl.create 32 in
Hashtbl.add t "hello" "world";
Hashtbl.add t "hello" "guys";
print_string @@ Hashtbl.find t "hello";
Hashtbl.remove t "hello";
print_string @@ Hashtbl.find t "hello"
  |};
  Tester.f
    {|
let t = Hashtbl.create 32 in
Hashtbl.add t "mlml" 1;
Hashtbl.replace t "mlml" 2;
print_int @@ Hashtbl.find t "mlml"
  |};
  Tester.f
    {|
let print_opt = function
  | Some v -> Printf.printf "Some %s\n" v
  | None -> print_string "None\n"
;;

let t = Hashtbl.create 32 in
print_opt @@ Hashtbl.find_opt t "mlml";
Hashtbl.add t "mlml" "oss";
print_opt @@ Hashtbl.find_opt t "mlml";
Hashtbl.remove t "mlml";
print_opt @@ Hashtbl.find_opt t "mlml"
  |};
  Tester.f
    {|
let t = Hashtbl.create 32 in
Hashtbl.add t "hello" "world";
let t2 = Hashtbl.copy t in
Hashtbl.replace t "hello" "yeah";
print_string @@ Hashtbl.find t2 "hello"
  |}
;;
