let () =
  Tester.expr "let x = 10 in x + 33";
  Tester.expr "let abc = 10 in abc + 33";
  Tester.expr {|
    let x = 10 in
    let y = 3 in
    x + y + 30
  |};
  Tester.expr {|
    let x = 10 in
    let x' = 3 in
    x + x' + 30
  |}
;;
