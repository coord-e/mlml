let () =
  Tester.expr "(fun x -> x + 4) 2";
  Tester.expr {|
let f = fun x -> x * 10 in
f 4
|};
  Tester.expr {|
let f = fun (a, b) -> a + b in
f (1, 2)
|}
;;
