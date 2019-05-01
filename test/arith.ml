let () =
  Tester.expr "13 + 5 * 10";
  Tester.expr "(2 + 5) * 6";
  Tester.expr "14 * 2 - 3 * 5";
  Tester.expr "1+2+3+4+5+6+7*2+8";
  Tester.expr "4 / 2";
  Tester.expr "12 / 5 + 3";
  Tester.expr "12 mod 5 * 14";
  Tester.expr "8 + 3 mod 12 + 4";
  Tester.bool_expr "1 > 2";
  Tester.bool_expr "133 < 123";
  Tester.bool_expr "42 > 42";
  Tester.bool_expr "2 < 42";
  Tester.bool_expr "42 >= 42";
  Tester.bool_expr "4 <= 5";
  Tester.bool_expr "5 <= 5";
  Tester.bool_expr "4 >= 8";
  Tester.bool_expr "false || false";
  Tester.bool_expr "true || true";
  Tester.bool_expr "true && true";
  Tester.bool_expr "false && true";
  Tester.bool_expr "false && (print_int 42; true)";
  Tester.bool_expr "true && (print_int 42; true)"
;;
