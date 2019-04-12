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
  Tester.bool_expr
    {|
let a = (4, (3, (3, 2), 3)) in
let b = (3, 2) in
let f v = (4, (v, b, v)) in
a = f 3
|};
  (* variant *)
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

(* with tuple *)
print_bool ((1, A (1, 2)) = (1, A (1, 2))) ;;
print_bool ((A (1, 2), 4) <> (B (1, 2), 4)) ;;
print_bool ((1, D) = (1, C 0)) ;;

(* complex *)
let f = function
  | 1 -> A (0, 0)
  | 2 -> B (0, 0)
  | x -> C x
in let g = function
  | x, y when x = (1, 2) -> y + 1
  | x, y when x = (3, 5) -> y + 3
  | _, y -> y
in
print_bool (f (g ((3, 5), 0)) = A (0, 0));
print_bool (f (g ((1, 5), 5)) <> C 5);
print_bool (f (g ((1, 2), 1)) = B (0, 0))
  |}
;;
