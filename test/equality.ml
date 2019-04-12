let () =
  (* Physical equality *)
  Tester.bool_expr "1 == 1";
  Tester.bool_expr "1 != 1";
  Tester.bool_expr "0 == 1";
  Tester.bool_expr "0 != 1";
  Tester.bool_expr "(1, 2, 3) == (1, 2, 3)";

  (* Structural equality *)
  Tester.bool_expr "2 = 2";
  Tester.bool_expr "0 <> 0";
  Tester.bool_expr "(1, 2) = (1, 2)";
  Tester.bool_expr "(1, (2, 3), 3) = (1, (2, 3), 3)";
  Tester.bool_expr "(1, 4, 3) = (1, 2, 3)";
  Tester.bool_expr "(4, 3) = (2, 3)";
  Tester.bool_expr "((2, 3), 5, (2, 6)) <> ((2, 3), 5, (2, 6))";
  Tester.bool_expr "(1, 5, (2, 6)) <> (1, 5, (2, 3))";
  Tester.bool_expr "(4, 3) <> (2, 3)";
