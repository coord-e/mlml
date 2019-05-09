let () =
  Tester.f {|print_string "hello, world!"|};
  Tester.f {|
let s1 = "hello" in
let s2 = "world" in
print_string (s1 ^ ", " ^ s2)
|};
  Tester.bool_expr
    {|
  let s1 = "hel" in
  let is_hello = function "hello" -> true | _ -> false in
  is_hello (s1 ^ "lo")
|};
  Tester.bool_expr {|
  "hello" = "hello"
  |};
  Tester.bool_expr {|
  "hell" <> "hello"
  |};
  Tester.bool_expr {|
  "hello" != "hello"
  |};
  Tester.f {|
  print_string "this is \"quoted\" string"
  |};
  Tester.f
    {|
let f = function 'a' -> "AHHHH" | 'b' -> "BEEEE" | _ -> "huh, idk" in
print_string (f "aieee".[0])
|};
  Tester.f
    {|
let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let rec print = function
  | [] -> print_string "end"
  | h :: t ->
      print_char h;
      print_string "->";
      print t
;;

print (explode "hello, world")
  |};
  Tester.f
    {|
type op =
  | Just of string
  | Concat of op * op
;;

let replace = function
  | "like" -> "love"
  | "bad" -> "cool"
  | s -> s
;;

let rec f = function
  | Just s -> replace s
  | Concat (a, b) -> f a ^ " " ^ f b
;;

let s = Concat (
  Concat (
    Just "I",
    Just "like"
  ),
  Concat (
    Concat (
      Just "bad",
      Concat (
        Just "and",
        Just "cute"
      )
    ),
    Just "dogs"
  )
) in print_string (f s)
  |}
;;
