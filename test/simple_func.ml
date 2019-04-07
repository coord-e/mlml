let () =
  Tester.expr "let f x = x + 3 in f 40";
  Tester.expr {|
    let f x = x + 3 in
    let g x = 10 * x in
    f (g 4)
  |};
  Tester.expr {|
    let f x = x + 3 in
    let g x = 10 * x in
    f 0 + g 4
  |}
;;
