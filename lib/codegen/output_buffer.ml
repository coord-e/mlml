type placeholder = Holder of int

type line =
  | Label of string
  | Inst of string
  | Placeholder of placeholder

type t =
  { mutable main : line list
  ; mutable sub : line list
  ; mutable placeholder_index : int }

let create () = {main = []; sub = []; placeholder_index = 0}

(* basic emit functions *)
let emit buf line = buf.main <- line :: buf.main
let emit_sub buf line = buf.sub <- line :: buf.sub

(* auxiliary functions *)
let emit_inst buf inst = emit buf (Inst inst)
let emit_sub_inst buf inst = emit_sub buf (Inst inst)

(* placeholder handlings *)
let emit_placeholder buf =
  let i = buf.placeholder_index in
  buf.placeholder_index <- i + 1;
  let p = Holder i in
  emit buf (Placeholder p);
  p
;;

let substitute buf holder line =
  let aux = function Placeholder l when l = holder -> line | l -> l in
  buf.main <- List.map aux buf.main
;;

let prepend_buffer a b =
  let a_idx = a.placeholder_index in
  let aux = function
    | Placeholder (Holder i) -> Placeholder (Holder (i + a_idx))
    | l -> l
  in
  a.main <- List.map aux b.main |> List.append a.main;
  a.sub <- List.map aux b.sub |> List.append a.sub;
  a.placeholder_index <- a_idx + b.placeholder_index
;;

let append_buffer a b = prepend_buffer b a

let contents buf =
  let aux = function
    | Label s -> s ^ ":"
    | Inst s -> "\t" ^ s
    | Placeholder _ -> failwith "subst is left"
  in
  List.rev buf.main |> List.rev_append buf.sub |> List.map aux |> String.concat "\n"
;;
