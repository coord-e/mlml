let () =
  (* in pervasives *)
  Tester.expr
    {|
let f g h = g 10 |> h in
let g x = x + 20 in
let fa a = g @@ a * 200 in
f g fa
  |};
  Tester.f
    {|
let l1 = [1; 2; 3]
let l2 = [4; 5; 6]
;;

let rec print_list = function
  | [] -> print_string "end\n"
  | h :: t -> print_int h; print_string "->"; print_list t
;;

print_list (l1 @ l2);
print_list (l1 @ [1; 3]);
print_list (l2 @ [10]);
print_list ([4; 5] @ [3; 4; 10])
  |};
  (* user-defined *)
  Tester.f
    {|
let (<=>) a b = a - b
let (-->) a b = a * b
let (@~@) a b = a + b
;;
print_int (50 <=> 8);
print_int (7 --> 6);
print_int (13 @~@ 29)
  |}
;;
