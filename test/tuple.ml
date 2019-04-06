let () =
  Tester.f "let a, b = 1, 2 in a + b";
  Tester.f {|
    let a = 1, 2, 3 in
    let x, y, z = a in
    x * y + z
  |};
  Tester.f
    {|
    let f a = a, a+1, a+2, a+3 in
    let a, b, c, d = f 10 in
    a + b + c + d
  |};
  Tester.f {|
    let a, (b, c) = 1, (2, 3) in
    a + b * c
  |};
  Tester.f {|
    let add (a, b) = a + b in
    add (1, 2)
  |}
;;
