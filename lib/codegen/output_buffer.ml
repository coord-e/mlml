type line =
  | Label of string
  | Inst of string
  | Placeholder of string

type t =
  { mutable main : line list
  ; mutable sub : line list }

let create () = {main = []; sub = []}

(* basic emit functions *)
let emit buf line = buf.main <- line :: buf.main
let emit_sub buf line = buf.sub <- line :: buf.sub

(* auxiliary functions *)
let emit_inst buf inst = emit buf (Inst inst)
let emit_sub_inst buf inst = emit_sub buf (Inst inst)
let emit_placeholder buf s = emit buf (Placeholder s)

let substitute buf f =
  let aux = function Placeholder l -> f l | l -> l in
  buf.main <- List.map aux buf.main
;;

let prepend_buffer a b =
  a.main <- a.main @ b.main;
  a.sub <- a.sub @ b.sub
;;

let append_buffer a b =
  a.main <- b.main @ a.main;
  a.sub <- b.sub @ a.sub
;;

let contents buf =
  let aux = function
    | Label s -> s ^ ":"
    | Inst s -> "\t" ^ s
    | Placeholder _ -> failwith "subst is left"
  in
  List.rev buf.main |> List.rev_append buf.sub |> List.map aux |> String.concat "\n"
;;
