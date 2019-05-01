let () =
  Tester.expr "13 + 5 * 10";
  Tester.expr "(2 + 5) * 6";
  Tester.expr "14 * 2 - 3 * 5";
  Tester.expr "1+2+3+4+5+6+7*2+8";
  Tester.expr "4 / 2";
  Tester.expr "12 / 5 + 3";
  Tester.expr "12 mod 5 * 14";
  Tester.expr "8 + 3 mod 12 + 4"
;;
