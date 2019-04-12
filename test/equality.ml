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
  Tester.f
    {|
type variant =
  | A of int * int
  | B of int * int
  | C of int
  | D

let print_bool = function
  | true -> print_int 1
  | false -> print_int 0
;;

(* Physical equality *)
print_bool (A (1, 2) == A (1, 2)) ;;
print_bool (A (6, 2) != B (6, 2)) ;;

(* Structural equality *)
print_bool (A (1, 2) = A (1, 2)) ;;
print_bool (A (3, 2) <> A (1, 4)) ;;
print_bool (A (3, 2) = A (1, 4)) ;;
print_bool (A (6, 2) = B (6, 2)) ;;
print_bool (C 1 = C 1) ;;
print_bool (A (5, 2) <> C 1) ;;
print_bool (A (0, 0) = D) ;;
print_bool (D = D) ;;
print_bool (D <> C 0) ;;
  |}
;;
