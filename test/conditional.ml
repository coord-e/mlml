let () =
  Tester.f "if true then 43 else 10";
  Tester.f "if false then 10 else 40 + 3";
  Tester.f {|
    if 1 = 2
    then 4
    else (
      let a = 10 in
      if a * 2 = 20
      then 43
      else 0
    )
  |}
;;
