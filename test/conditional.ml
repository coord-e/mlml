let () =
  Tester.f "if true then 43 else 10";
  Tester.f "if false then 10 else 40 + 3";
  Tester.f {|
    if 1 = 2
    then 20
    else 43
  |}
;;
