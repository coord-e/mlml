let () =
  Tester.bool_expr {| Filename.is_relative "a/b" |};
  Tester.bool_expr {| Filename.is_relative "/a/b" |};
  Tester.bool_expr {| Filename.is_relative "./a/b" |};
  Tester.f {|
print_endline @@ Filename.chop_suffix "./a/b.ml" ".ml"
  |};
  Tester.bool_expr {| Filename.check_suffix "./a/b.ml" ".ml" |};
  Tester.bool_expr {| Filename.check_suffix "a/b.ml/c" ".ml" |};
  Tester.f {|
print_endline @@ Filename.basename "./a/b/c.ml"
  |};
  Tester.f {|
print_endline @@ Filename.basename "./a/b/c/"
  |};
  Tester.f {|
print_endline @@ Filename.basename "./a/b"
  |};
  Tester.f {|
print_endline @@ Filename.dirname "./a/b/c.ml"
  |};
  Tester.f {|
print_endline @@ Filename.dirname "./a/b/c/"
  |};
  Tester.f {|
print_endline @@ Filename.dirname "./a/b"
  |}
;;
