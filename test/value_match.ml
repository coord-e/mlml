let () =
  Tester.expr
    {|
    let f = function
      | 1 -> 3
      | 2 -> 4
      | 3 -> 5
      | _ -> 6
    in (f 2) + (f 4)
  |};
  Tester.expr
    {|
    let f = function
      | 3, 2 -> 3
      | 4, a -> a + 1
      | a, b -> a + b
    in (f (4, 5)) + (f (3, 2)) + (f (9, 8))
  |};
  Tester.f
    {|
    type t =
      | A of int
      | B of int * int
    ;;
    let f = function
      | A 1 -> 42
      | A x -> x
      | B (1, x) -> x
      | B (x, 1) -> x + 5
      | _ -> 99
    in
    print_int (f (A 4));
    print_int (f (A 1));
    print_int (f (B (1, 5)));
    print_int (f (B (9, 1)));
    print_int (f (B (10, 10)))
  |}
;;
