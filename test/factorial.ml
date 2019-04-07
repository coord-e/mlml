let () =
  Tester.expr
    {|
    let rec factorial n =
      if n = 0
      then 1
      else n * factorial (n - 1)
    in factorial 5
  |}
;;
