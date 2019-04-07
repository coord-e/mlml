let () =
  Tester.f
    {|
type t = A | B | C
;;

let v = A in
  let r = (
    match v with
    | A -> 1
    | B -> 2
    | C -> 3
  ) in print_int r
  |};
  Tester.f
    {|
type t =
  | Int of int
  | Add of t * t
  | Mul of t * t

let rec calc = function
  | Int a -> a
  | Add (a, b) ->
    let a = calc a in
    let b = calc b in
    a + b
  | Mul (a, b) ->
    let a = calc a in
    let b = calc b in
    a * b
;;

;;
let tree = Mul (Add (Int 3, Int 5), Mul (Int 2, Int 4)) in
print_int (calc tree)
  |};
  Tester.f
    {|
type ta = A | B
type tb =
  | C of ta * int

let m = function
  | C (A, i) -> i
  | C (B, i) -> i + 1
;;

print_int (m (C (B, 10)))
  |};
  Tester.f
    {|
type ta =
  | A of int * int
  | B of int

type tb =
  | C of ta * (int * int)
  | D

let complex_match = function
  | C (A (a, b), t) ->
      let c, d = t in
      a + b + c + d
  | C (B i, (a, b)) -> i + a * b
  | D -> 10
;;

let a = complex_match (C (A (4, 5), (6, 7))) in
let b = complex_match (C (B 12, (3, 4))) in
print_int (a + b)
  |}
;;
