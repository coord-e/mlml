type t =
  { mutable main : string list
  ; mutable sub : string list }

let create () = {main = []; sub = []}

let emit_instruction buf inst =
  let s = Printf.sprintf "\t%s" inst in
  buf.main <- s :: buf.main
;;

let contents buf = List.append buf.sub buf.main |> String.concat "\n"
