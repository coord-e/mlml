let () =
  Tester.expr {|
let v = 1 :: 2 :: 3 :: [] in
let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
in length v
  |};
  Tester.f {|
let v1 = [1; 2; 3] ;;
let v2 = [4; 2; 9; 10;] ;;
let rec print_list = function
  | [] -> print_int 0
  | h :: t -> print_int h; print_list t
and app a b = match a with
  | [] -> b
  | h :: t -> h :: (app t b)
and rev = function
  | [] -> []
  | h :: t -> app (rev t) [h]
;;
print_list (rev (app v1 v2))
  |};
  Tester.expr {|
let v1 = [(1, 2); (2, 3); (3, 4)] in
let rec f = function
  | [] -> 0
  | (x, y) :: t -> x + y + (f t)
in f v1
  |};
  Tester.expr {|
let l = [1; 2; 3; 2; 3] in
let rec f l =
  match l with
  | a :: b :: t -> a * b + (f t)
  | _ -> 0
in f l
  |};
  Tester.f {|
type t =
  | A
  | G
  | C
  | T
;;

let l1 = [A; T; G; C; A; T; G; A; C; T; A; A] ;;
let l2 = [G; A; C; T; A; T; G; C; A; T; T; A; A] ;;
let rec parse = function
  | A :: T :: G :: rest ->
      let rest, acc = parse rest in
      rest, 1 :: acc
  | C :: A :: T :: rest ->
      let rest, acc = parse rest in
      rest, 2 :: acc
  | G :: A :: C :: rest ->
      let rest, acc = parse rest in
      rest, 3 :: acc
  | T :: A :: A :: rest -> rest, [0]
  | _ :: t -> parse t
  | [] -> [], []
;;

let rec print_list = function
  | [] -> print_int 0
  | h :: t -> print_int h; print_list t
;;

let print_result r =
  let _, l = r in
  print_list l
;;

print_result (parse l1) ;;
print_result (parse l2) ;;
  |};
  Tester.expr {|
let rec f = function
  | [a; b] -> a + b
  | [1] -> 10
  | [x] -> x
  | h :: t -> f [h] + f t
  | _ -> 100
in
f [3; 1; 5; 2; 1; 7; 1; 10]
  |}
