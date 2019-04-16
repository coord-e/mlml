let () =
  Tester.expr {|
let v = 1 :: 2 :: 3 :: [] in
let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
in length v
  |}

