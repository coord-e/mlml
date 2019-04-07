let () =
  Tester.expr {|
    match 2 with
    | 1 -> 3
    | 2 -> 4
    | 3 -> 5
  |};
  Tester.expr
    {|
    let a = 3 in
    match a with
    | 1 -> 3
    | 2 -> 4
    | 3 -> 5
  |};
  Tester.expr
    {|
    let a = 4 in
    match a with
    | 1 -> 3
    | 2 -> 4
    | 3 -> 5
    | _ -> 6
  |}
;;
