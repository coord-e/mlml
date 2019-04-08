let () =
  Tester.expr
    {|
    let f = function
      | 1 | 2 -> 3
      | 3 -> 5
      | _ -> 6
    in (f 2) + (f 1) + (f 4)
  |};
  Tester.f
    {|
    type t =
      | A of int * int
      | B of int * int

    let f = function
      | A (a, b) | B (a, b) -> a + b
    ;;
    print_int (f (A (2, 3)));
    print_int (f (B (4, 5)))
      |};
  Tester.f
    {|
    type t =
      | A of int * int
      | B of int

    let f = function
      | A (a, _) | B a -> a + 1
    ;;
    print_int (f (A (2, 3)));
    print_int (f (B 4))
      |};
  Tester.f
    {|
    type t =
      | A of int * int
      | B of int * int
      | C of int * int
      | D of int

    let f = function
      | A (a, b) | B (a, b) | C (a, b) -> a + b
      | D i -> i * 100
    ;;
    print_int (f (A (2, 3)));
    print_int (f (B (4, 5)));
    print_int (f (C (6, 7)));
    print_int (f (D 7))
      |}
;;
