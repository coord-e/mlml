let () =
  (* starting with underscore to suppress warning *)
  Tester.expr {|
    let _f x = x + 3 in
    let _f x = 10 * x + 3 in
    _f 4
  |};
  Tester.expr
    {|
    let f x = (
      if x = 0
      then x + 3
      else x * 3
    ) in
    let g x = (
      if x = 9
      then x + 3
      else x * 3
    ) in
    g (f 3)
  |}
;;
