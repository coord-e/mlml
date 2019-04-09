let () =
  Tester.expr "1 + (* comment *) 2";
  Tester.expr
    {|
    let (* comment *) f x = x + 1 (* comment *) in
    f (* comment *) 2
  |};
  Tester.f
    {|
  (* comment *)
let f (* comment *) x = x + 1 (* comment *) ;;
print_int (f 2)
|}
;;
