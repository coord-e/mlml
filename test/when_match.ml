let () =
  Tester.f
    {|
    let f v =
      let cond = function
        | 1 | 2 | 3 -> true
        | _ -> false
      in match v with
      | x when cond x -> 42
      | x when x = 10 -> 10
      | _ -> 0
    in
    print_int (f 1);
    print_int (f 3);
    print_int (f 10);
    print_int (f 11)
      |};
  Tester.f
    {|
    let f = function
      | x, y when x = y -> 42
      | x, y -> x + y
    in
    print_int (f (1, 2));
    print_int (f (10, 10))
      |};
  Tester.f
    {|
    type t =
      | A of int * int
      | B of int * int

    let f = function
      | A (a, b) | B (a, b) when a + b = 20 -> a * b
      | A (a, b) -> a + b
      | B (a, b) -> a - b
    ;;
    print_int (f (A (2, 3)));
    print_int (f (B (15, 5)));
    print_int (f (A (4, 16)));
    print_int (f (B (5, 3)))
      |}
;;
