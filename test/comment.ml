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
|};
  Tester.f
    {|
  (* com (* comment *) ent *)
  (* (* comment *) *)
let f (* comment (*comment*) *) x = x + 1 (*(*comment*) comment *) ;;
print_int (f (** comment **) 2)
|}
;;
