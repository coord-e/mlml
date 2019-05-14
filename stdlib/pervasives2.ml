let string_of_int i =
  let string_of_pos i =
    let char_of_digit d = char_of_int (int_of_char '0' + d) in
    let rec len = function i when i < 10 -> 1 | i -> 1 + len (i / 10) in
    let rec digit_of n = function
      | 0 -> char_of_digit @@ (n mod 10)
      | i -> digit_of (n / 10) (i - 1)
    in
    let l = len i in
    let f idx = digit_of i @@ (l - idx - 1) in
    String.init l f
  in
  if i < 0 then "-" ^ string_of_pos (-i) else string_of_pos i
;;

module MlmlInternalFormat = struct
  let fmt_int d = string_of_int d
  let fmt_string s = s
  let fmt_char c = String.make 1 c
end
