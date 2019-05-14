let () =
  Tester.expr "let a, b = 1, 2 in a + b";
  Tester.expr {|
    let a = 1, 2, 3 in
    let x, y, z = a in
    x * y + z
  |};
  Tester.expr
    {|
    let f a = a, a+1, a+2, a+3 in
    let a, b, c, d = f 10 in
    a + b + c + d
  |};
  Tester.expr {|
    let a, (b, c) = 1, (2, 3) in
    a + b * c
  |};
  Tester.expr {|
    let add (a, b) = a + b in
    add (1, 2)
  |};
  Tester.expr
    {|
    let add (a, b, (d, e)) = a * b + d * e in
    let v, t = 10, (3, 4) in
    add (5, v, t)
  |};
  Tester.expr {|
let t = (1, fun x -> x + 1) in
let v, f = t in
f v
  |}
;;
