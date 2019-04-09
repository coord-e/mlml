let () =
  Tester.expr "let f a b c = a + b * c in f 1 3 5";
  Tester.expr {|
let a = 10 in
let f x = a + x in
f 5
|};
  Tester.expr
    {|
let f a b c = a * b + c in
let add = f 1 in
let succ = add 1 in
f (add (succ 2) 3) (succ 5) (add 2 3)
|};
  (* TODO: ack with larger number *)
  Tester.expr
    {|
let rec ack m n =
  match m, n with
  | 0, n -> n + 1
  | m, 0 -> ack (m - 1) 1
  | m, n -> ack (m - 1) (ack m (n - 1))
in ack 3 5
  |};
  Tester.expr {|
let f x = x + 1 in
let g x = f x + 1 in
g 10
  |};
  Tester.expr
    {|
let t = (2, 3), 4 in
let f x =
  let (a, b), c = t in
  a * b + c * x
in f 10
  |};
  Tester.expr {|
let f (a, b) c =
  a * b + c
in
let g = f (2, 3) in
g 10
  |};
  Tester.f {|
let v = 10
let f x = v + x
;;
print_int (f 12)
  |};
  Tester.f {|
let f a b = a + b ;;
let succ = f 1 ;;
print_int (succ 12)
  |};
  Tester.f
    {|
let v = 1 ;;
let rec fact = function
  | 0 -> v
  | n -> n * fact (n - 1)
;;

print_int (fact 5)
  |};
  Tester.f {|
let printer = print_int in
printer 12
  |}
;;
