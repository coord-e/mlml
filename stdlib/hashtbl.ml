(* fake implementation of Hashbl *)
(* this behaves like OCaml's `Hashtbl`, but is not a hash table at all *)

type ('a, 'b) t = {mutable data : ('a * 'b) list}

let create _ = {data = []}
let clear t = t.data <- []
let reset = clear
let copy t = {data = t.data}
let update t f = t.data <- f t.data
let add t k v = update t @@ List.cons (k, v)
let find t k = List.assoc k t.data
let find_opt t k = List.assoc_opt k t.data
let mem t k = List.mem_assoc k t.data
let remove t x = update t @@ List.remove_assoc x

let replace t k v =
  remove t k;
  add t k v
;;
