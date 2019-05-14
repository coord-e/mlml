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

and t2 =
  { f : string
  ; s : string
  ; t : string }

and t3 =
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

print_int (f v) ;;
print_string (g v) ;;
print_string (g {v with value = {a = "string"; b = 20, "str"}, 1}) ;;
print_string (g {v with sub = {v.sub with t = "o, mlml"}})
  |};
  Tester.f
    {|
type 'a t =
  { lst : 'a t list
  ; v : 'a }

let rec f v =
  let rec aux = function h :: t -> (aux h.lst * h.v) + aux t | [] -> v.v in
  aux v.lst
;;

let value = {lst = [{lst = [{lst = []; v = 1}]; v = 10}; {lst = []; v = 40}]; v = 20}

;;
print_int (f value)
  |};
  (* mutable fields *)
  Tester.f
    {|
type r = { mutable a : int; mutable b : string }
let print_r {a; b} = Printf.printf "a = %d, b = %s\n" a b
let r = { a = 10; b = "hello" }
;;
print_r r;
r.a <- 20;
print_r r;
r.b <- "hi";
print_r r
  |};
  Tester.f
    {|
type r = { a : int; mutable b : string } ;;
let r = { a = 10; b = "hello" } in
let s = r in
r.b <- "world";
print_string s.b;
let t = { r with a = 30 } in
r.b <- "mlml";
print_string t.b
  |};
  Tester.f
    {|
type t0 = { deep_value : string }
type t1 = { inner_value : int; t0 : t0 }
type t2 = { value : t1 }
;;
let v = { value = { inner_value = 41; t0 = { deep_value = "Hello" }}} in
print_int v.value.inner_value;
print_string v.value.t0.deep_value;
print_char v.value.t0.deep_value.[2]
  |}
;;
