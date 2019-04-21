let () =
  Tester.f
    {|
let f = function
  | 'a' .. 'd' -> 0
  | 'd' .. 'g' -> 1
  | _ -> 2
in
print_int (f 'a');
print_int (f 'c');
print_int (f 'd');
print_int (f 'g');
print_int (f 'x')
      |};
  Tester.f
    {|
let is_digit = function
  | '0' .. '9' -> 1
  | _ -> 0
;;

print_int (is_digit 'a');
print_int (is_digit '0');
print_int (is_digit '7');
print_int (is_digit '9');
print_int (is_digit 'd')
  |};
  Tester.f
    {|
let is_uppercase = function
  | 'A' .. 'Z' -> 1
  | _ -> 0
;;

print_int (is_uppercase '9');
print_int (is_uppercase 'A');
print_int (is_uppercase 'c');
print_int (is_uppercase 'D');
print_int (is_uppercase '_');
print_int (is_uppercase 'Z')
  |}
;;
