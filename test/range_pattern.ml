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
  | '0' .. '9' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let print_bool =
  | true -> print_string "true\n"
  | false -> print_string "false\n"
;;

print_bool (is_digit 'a');
print_bool (is_digit '0');
print_bool (is_digit '7');
print_bool (is_digit '9');
print_bool (is_digit 'd');
print_bool (is_uppercase '9');
print_bool (is_uppercase 'A');
print_bool (is_uppercase 'c');
print_bool (is_uppercase 'D');
print_bool (is_uppercase '_');
print_bool (is_uppercase 'Z')
      |}
;;
