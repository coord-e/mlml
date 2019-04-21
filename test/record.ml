let () =
  Tester.f
    {|
type t =
  { a : int
  ; b : int
  ; c : string }
;;

let v = {a = 1; c = "hello!"; b = 2} in
print_int v.a;
print_int v.b;
print_string v.c
|};
  Tester.f
    {|
type t =
  { a : int
  ; b : string }

let create a = {a; b = "hello"}
let modify v s = {v with b = s}
;;

let v = create 7 in
match modify v "world" with {a; b = c} -> print_int a; print_string c
|};
  Tester.f
    {|
type t1 =
  { a : string
  ; b : int * string }

type t2 =
  { f : string
  ; s : string
  ; t : string }

type t3 =
  { value : t1 * int
  ; sub : t2 }

let v = {value = {a = "hello"; b = 10, "he"}, 10; sub = {f = "l"; s = "l"; t = "o"}}

let f v =
  let vf, vs = v.value in
  let bf, bs = vf.b in
  let {f = sf; s = ss; t = _} = v.sub in
  let s = bs ^ sf ^ ss in
  if s = vf.a then bf + vs else bf * vs
;;

let g = function
  | {value = {a; b = 10, b}, _; sub = {f; s; t}} -> a ^ b ^ f ^ s ^ t
  | _ -> "mlml"
;;

print_int (f v)
print_string (g v)
print_string (g {v with value = {a = "string"; b = 20, "str"}, 1})
print_string (g {v with sub = {v.sub with t = "o, mlml"}})
|}
;;
