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
  |};
  Tester.f
    {|
let t = Hashtbl.create 32 in
Hashtbl.add t "hello" "world";
Hashtbl.add t "hello" "guys";
Hashtbl.iter (Printf.printf "%s -> %s\n") t
  |};
  Tester.f
    {|
let t = Hashtbl.create 32 in
Hashtbl.add t "hello" "world";
Hashtbl.add t "fantastic" "ramen";
Hashtbl.remove t "hello";
print_string @@ Hashtbl.find t "fantastic"
  |};
  Tester.f
    {|
let t = Hashtbl.create 32 in
Hashtbl.add t "hello" "world";
Hashtbl.add t "fantastic" "ramen";
let f k v acc = (k ^ v) :: acc in
let l = Hashtbl.fold f t [] in
List.iter print_endline l
  |};
  (* List *)
  Tester.f
    {|
let add a b = a + b in
let sum = List.fold_left add 0 in
let test l = print_int @@ sum l in
test [3; 4; 5; 6];
test [0; 2; 0];
test [1200; 123; 100]
  |};
  Tester.f
    {|
let f x acc = print_int x; x :: acc in
let fold l = List.fold_right f l [] in
let test l = List.iter print_int @@ fold l in
test [3; 4; 5; 6];
test [0; 2; 0];
test [1200; 123; 100]
  |};
  Tester.f
    {|
let test l x =
  if List.mem x l
  then print_string "found\n"
  else print_string "notfound\n"
in
test [1; 2; 3] 0;
test [2] 2;
test [] 0;
test [2; 3] 5
  |};
  Tester.f
    {|
let square x = x * x in
let mul (a, b) = a * b in
let test f l = List.iter print_int @@ List.map f l in
test succ [2; 3; 1; 3];
test square [4; 12; 23; 0];
test mul [9, 6; 5, 6; 1, 2; 0, 3; 3, 4]
  |};
  Tester.f
    {|
let square x = x * x in
let mul (a, b) = a * b in
let test f l = List.iter print_int @@ List.rev_map f l in
test succ [2; 3; 1; 3];
test square [4; 12; 23; 0];
test mul [9, 6; 5, 6; 1, 2; 0, 3; 3, 4]
  |};
  Tester.f
    {|
let f i x = i * x in
let test l = List.iter print_int @@ List.mapi f l in
test [143; 21; 34];
test [0; 6; 3; 2]
  |};
  Tester.f
    {|
let print = List.iteri (Printf.printf "%d -> %d, ") in
print @@ List.sort compare [5; 2; 4; 6; 1; 6; 7; 12; 4; 2; 0]
  |};
  Tester.f
    {|
let cmp (a1, b1) (a2, b2) = compare (a1 * b1) (a2 * b2) in
let print = List.iter (fun (a, b) -> Printf.printf "%d %d, " a b) in
print @@ List.sort cmp [9, 6; 5, 6; 1, 2; 0, 3; 3, 4]
  |};
  Tester.f
    {|
let test a b =
  if (List.rev a @ b <> List.rev_append a b)
  then print_string "differs"
in
test [1; 2; 5; 3] [12; 3; 4];
test [2; 3] [1; 3; 2]
  |};
  Tester.f
    {|
let test f l =
  let a, b = List.partition f l in
  List.iter print_int (a @ [0] @ b)
in
test (fun x -> x > 3) [4; 2; 5; 12; 0; 2]
  |};
  Tester.f
    {|
let a, b = List.split [4, 2; 5, 12; 0, 2; 5, 1; 12, 4] in
List.iter print_int a;
List.iter print_int b
  |};
  Tester.f
    {|
let test l = List.flatten l |> List.iter print_int in
test [[4; 2]; [5]; [12; 0; 2]];
test [[]; [2]; [0]; []; [1; 3]]
  |};
  Tester.f
    {|
let f = Printf.printf "%d, %s" in
List.iter2 f [1; 2; 3] ["hi"; "hello"; "omg"]
  |};
  (* String *)
  Tester.f
    {|
let test s =
  let aux c = match c with 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' -> c | _ -> '_' in
  print_string @@ String.map aux s
in
test "hello";
test "2ks43(as";
test "2\n3 as"
  |};
  Tester.f
    {|
print_string @@ String.uppercase_ascii "Hello World";
print_string @@ String.lowercase_ascii "iOS 10";
print_string @@ String.capitalize_ascii "hello";
print_string @@ String.uncapitalize_ascii "HELLO"
  |};
  Tester.f
    {|
let test s = print_string @@ String.escaped s in
test "hehehe";
test "hello world\nthis is mlml";
test "mlml\t@\tOCaml";
test "\"hello, world\""
  |};
  Tester.f
    {|
let test c =
  match String.index_opt "hello, world" c with
  | Some i -> print_int i
  | None -> print_string "none"
in
test ',';
test 'e';
test 'h';
test 'd';
test 'o'
  |};
  Tester.f
    {|
let test c s =
  List.iter (Printf.printf "%s, ") @@ String.split_on_char c s
in
test ',' "hello,world,mlml";
test ' ' "happy new  year";
test '.' "...this is mlml. mlml has stdlib."
  |};
  Tester.f {|
["hello"; "world"; "mlml"]
|> String.concat ", "
|> print_endline
  |};
  Tester.f {|
print_endline @@ String.escaped ""
  |};
  (* Unit operations *)
  Tester.f {|
let f x = print_int x; x ;;
let () = ignore @@ f 42
  |}
;;
