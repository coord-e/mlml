let () =
  Tester.expr "13 + 5 * 10";
  Tester.expr "(2 + 5) * 6";
  Tester.expr "14 * 2 - 3 * 5";
  Tester.expr "1+2+3+4+5+6+7*2+8"
;;
