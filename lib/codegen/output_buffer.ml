type t =
  { main : Buffer.t
  ; sub : Buffer.t }

let create size = {main = Buffer.create size; sub = Buffer.create size}

let emit_instruction buf inst =
  Buffer.add_char buf.main '\t';
  Buffer.add_string buf.main inst;
  Buffer.add_char buf.main '\n'
;;

let length buf = Buffer.length buf.main + Buffer.length buf.sub

let contents buf =
  let b = Buffer.create (length buf) in
  Buffer.add_buffer b buf.sub;
  Buffer.add_buffer b buf.main;
  Buffer.contents b
;;
