let () =
  Tester.bool_expr
    {|
let rec is_even = function
  | 0 -> true
  | x -> is_odd (x-1)
and is_odd = function
  | 0 -> false
  | x -> is_even (x-1)
in is_even 13
  |};
  Tester.expr
    {|
let rec f x = h (x - 1)
and g = function
  | 0 -> 0
  | x -> 1 + g (x - 1)
and h x = g x
in f 10
  |};
  Tester.expr {|
let rec f x = x + a
and a = 10
and g x = f x
in g 10
  |};
  Tester.expr {|
let f x = x + 1
and a, b = 1, 2
and g x = x * 10
in f a + g b
  |}
;;
