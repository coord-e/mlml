let rec length = function [] -> 0 | _ :: t -> 1 + length t
let cons h t = h :: t
let hd = function h :: _ -> h | [] -> failwith "hd"
let tl = function _ :: t -> t | [] -> failwith "tl"

let rec nth_opt l i =
  match l, i with h :: _, 0 -> Some h | _ :: t, i -> nth_opt t (i - 1) | _ -> None
;;

let nth l i = match nth_opt l i with Some v -> v | None -> failwith "nth"
let rec rev_append a b = match a with [] -> b | h :: t -> rev_append t (h :: b)
let rev l = rev_append l []
let append a b = a @ b
let rec concat = function [] -> [] | h :: t -> h @ concat t
let flatten = concat

let rec iter f = function
  | [] -> ()
  | h :: t ->
    f h;
    iter f t
;;

let iteri f =
  let rec aux i = function
    | [] -> ()
    | h :: t ->
      f i h;
      aux (i + 1) t
  in
  aux 0
;;

let rec map f = function [] -> [] | h :: t -> f h :: map f t

let mapi f =
  let rec aux i = function [] -> [] | h :: t -> f i h :: aux (i + 1) t in
  aux 0
;;

let rec fold_left f acc = function [] -> acc | h :: t -> fold_left f (f acc h) t

let rec fold_right f l acc =
  match l with [] -> acc | h :: t -> f h (fold_right f t acc)
;;

let rec mem x = function [] -> false | h :: _ when h = x -> true | _ :: t -> mem x t

let rec find_opt f = function
  | [] -> None
  | h :: _ when f h -> Some h
  | _ :: t -> find_opt f t
;;

let find f l = match find_opt f l with Some v -> v | None -> failwith "NotFound"

let rec filter f = function
  | [] -> []
  | h :: t when f h -> h :: filter f t
  | _ :: t -> filter f t
;;

let rec partition f = function
  | [] -> [], []
  | h :: t ->
    let a, b = partition f t in
    if f h then h :: a, b else a, h :: b
;;

let rec split = function
  | [] -> [], []
  | (a, b) :: t ->
    let a_l, b_l = split t in
    a :: a_l, b :: b_l
;;

let rec iter2 f l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | h1 :: t1, h2 :: t2 ->
    f h1 h2;
    iter2 f t1 t2
  | _, _ -> failwith "List.iter2"
;;

(* quick sort *)
let rec sort pred = function
  | [] -> []
  | h :: t ->
    let is_smaller x = pred h x >= 0 in
    let is_greater x = not (is_smaller x) in
    let smaller = sort pred @@ filter is_smaller t in
    let greater = sort pred @@ filter is_greater t in
    smaller @ [h] @ greater
;;

let assoc_opt a l =
  let f (k, _) = k = a in
  match find_opt f l with Some (_, v) -> Some v | None -> None
;;

let assoc a l = match assoc_opt a l with Some v -> v | None -> failwith "NotFound"
let mem_assoc a l = match assoc_opt a l with Some _ -> true | None -> false

let rec remove_assoc k = function
  | [] -> []
  | (a, b) :: t -> if a = k then t else remove_assoc k t
;;
