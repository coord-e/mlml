let () =
  Tester.f {|
;;
print_int 10
;;
  |};
  Tester.f {|
let var = 1 ;;
print_int var
  |};
  Tester.f {|
let f a = a + 1 ;;
let v = f 2 ;;
let g x = print_int x ;;
let _ = g v
  |};
  Tester.f {|
let a = 1 ;;
let b = a in
print_int b
  |}
;;
