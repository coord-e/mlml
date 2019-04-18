type line =
  | Inst of string
  | Subst of string

type t =
  { mutable main : line list
  ; mutable sub : line list }

let create () = {main = []; sub = []}
let emit_instruction' buf inst = buf.main <- Inst inst :: buf.main

let emit_instruction buf inst =
  let s = Printf.sprintf "\t%s" inst in
  emit_instruction' buf s
;;

let emit_substitution buf label = buf.main <- Subst label :: buf.main

let substitute buf f =
  let aux = function Subst l -> Inst (f l) | l -> l in
  buf.main <- List.map aux buf.main
;;

let append_buffer a b =
  a.main <- a.main @ b.main;
  a.sub <- a.sub @ b.sub
;;

let prepend_buffer a b =
  a.main <- b.main @ a.main;
  a.sub <- b.sub @ a.sub
;;

let contents buf =
  let aux = function Inst s -> s | Subst _ -> failwith "subst is left" in
  List.append buf.sub buf.main |> List.map aux |> String.concat "\n"
;;
