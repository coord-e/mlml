let () = (
  Tester.f "let x = 10 in x + 33";
  Tester.f "let x = 10 in let y = 3 in x + y + 30"
)
