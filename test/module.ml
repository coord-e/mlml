let () =
  Tester.f
    {|
module M = struct
  let f x = x + 1
  let v = 42
end

let f x = x * 10 ;;
print_int (M.f (f M.v))
  |};
  Tester.f
    {|
module Long_name = struct
  let f x = x * 42
  module So_long_name = struct
    let f x = x + 42
  end
end

module L = Long_name
module S = L.So_long_name
;;

print_int (L.f (S.f 42))
  |};
  Tester.f
    {|
module M = struct
  let f x = x + 1
  let v = 42
end

(* will be overwritten *)
let f x = x * 10

open M ;;

print_int (f v)
  |};
  Tester.f
    {|
module M = struct
  type t =
    | A
    | B
    | C

  type r =
    { a : t
    ; b : int }

  let f = function A -> 1 | B -> 2 | C -> 3
end

type t = A of M.r

;;
let f (A {M.a; M.b}) =
  let i = M.f a + b in
  print_int i
in
f (A {M.a = M.A; M.b = 10})
  |};
  Tester.f
    {|
    module M = struct
      type t =
        | A
        | B
        | C
    end

      open M

      ;;
      let f = function
        | A -> 1
        | B -> 2
        | C -> 3
      in print_int (f A)
  |}
;;
