let () =
  Tester.f "if true then print_int 1";
  Tester.f {|
let b = true in
if b
then print_int 1;
if not b
then print_int 2
  |};
  Tester.f {|
let () =
  print_string "hello";
  print_string "world"
  |};
  Tester.f
    {|
let rec f = function
  | 0 -> ()
  | i -> print_int i; f @@ i - 1
in f 10
  |}
;;
