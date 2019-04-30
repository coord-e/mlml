let () =
  Tester.f
    {|
let b = Bytes.of_string "hello" in
Bytes.set b 1 'o';
print_string @@ Bytes.to_string b;
print_char @@ Bytes.get b 1;
print_char @@ Bytes.get b 4
  |};
  Tester.f
    {|
let b1 = Bytes.of_string "hello" in
let b2 = Bytes.copy b1 in
Bytes.set b1 1 'o';
print_string @@ Bytes.to_string b2
  |};
  Tester.f
    {|
let b1 = Bytes.of_string "hello, world" in
let b2 = Bytes.of_string "that curry is delicious" in
Bytes.blit b2 6 b1 1 4;
print_string @@ Bytes.to_string b1
  |};
  Tester.f {|
let b = Bytes.make 10 'w' in
print_string @@ Bytes.to_string b
  |};
  Tester.f {|
let s = "ramen" in
let sub = String.sub s 2 3 in
print_string sub
  |};
  Tester.f
    {|
let char_of_digit d = char_of_int (int_of_char '0' + d) ;;
let f i = char_of_digit i in
let s = String.init 10 f in
print_string s
  |}
;;
