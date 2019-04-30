module B = Output_buffer
open Builder

let match_fail ctx buf _label _ret_label =
  (* emit data *)
  let str_label = new_label ctx ".string_of_match_fail" in
  B.emit_sub buf (B.Label (string_of_label str_label));
  B.emit_sub_inst buf ".string \"runtime error: patten match failed. aborted.\"";
  B.emit_sub_inst buf ".fill 3";
  (* emit function body *)
  let a1, free1 = nth_arg_register ctx 0 in
  label_ptr_to_register buf str_label a1;
  let _ = safe_call ctx buf "puts@PLT" [RegisterValue a1] in
  free1 ctx;
  assign_to_register buf (ConstantValue 1) ret_register;
  let _ = safe_call ctx buf "exit@PLT" [RegisterValue a1] in
  ()
;;

let print_int ctx buf _label _ret_label =
  (* emit data *)
  let str_label = new_label ctx ".string_of_print_int" in
  B.emit_sub buf (B.Label (string_of_label str_label));
  B.emit_sub_inst buf ".string \"%ld\"";
  (* emit function body *)
  let a1, free1 = nth_arg_register ctx 0 in
  let a2, free2 = nth_arg_register ctx 1 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a2) (-8);
  restore_marked_int buf (RegisterValue a2);
  label_ptr_to_register buf str_label a1;
  B.emit_inst buf "xorq %rax, %rax";
  let _ = safe_call ctx buf "printf@PLT" [RegisterValue a1; RegisterValue a2] in
  free1 ctx;
  free2 ctx
;;

let print_char ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  restore_marked_int buf (RegisterValue a1);
  let _ = safe_call ctx buf "putchar@PLT" [RegisterValue a1] in
  free1 ctx
;;

let print_string ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  let a2, free2 = nth_arg_register ctx 1 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* assume reg is a pointer to string value *)
  string_value_to_content ctx buf (RegisterValue a1) (RegisterValue a1);
  B.emit_inst_fmt buf "movq stdout(%%rip), %s" (string_of_register a2);
  let _ = safe_call ctx buf "fputs@PLT" [RegisterValue a1; RegisterValue a2] in
  free1 ctx;
  free2 ctx
;;

let equal ctx buf label ret_label =
  (* TODO: Enable to take expected result and return earlier *)
  let arg1, free1 = nth_arg_register ctx 0 in
  let arg2, free2 = nth_arg_register ctx 1 in
  (* assume arg1 and arg2 are values of the same type *)
  (* TODO: Check the value type of two values *)
  let direct_label = new_unnamed_label ctx in
  branch_if_not_pointer ctx buf (RegisterValue arg1) direct_label;
  (* pointer comparison branch (recursion) *)
  let num = StackValue (push_to_stack ctx buf (ConstantValue 0)) in
  let count = alloc_register ctx in
  assign_to_register buf (ConstantValue 0) count;
  (* assume arg1 and arg2 has the same number of values *)
  read_from_address ctx buf (RegisterValue arg1) num 0;
  (* save raw num value to identify comparison mode *)
  let num_tmp_reg = alloc_register ctx in
  assign_to_register buf num num_tmp_reg;
  restore_marked_int buf num;
  let loop_label = new_unnamed_label ctx in
  let compare_label = new_unnamed_label ctx in
  let v1 = alloc_register ctx in
  assign_to_register buf (RegisterValue arg1) v1;
  let v2 = alloc_register ctx in
  assign_to_register buf (RegisterValue arg2) v2;
  start_label buf loop_label;
  branch_by_comparison ctx buf Eq num (RegisterValue count) ret_label;
  B.emit_inst_fmt buf "addq $8, %s" (string_of_register count);
  B.emit_inst_fmt buf "subq $8, %s" (string_of_register v1);
  B.emit_inst_fmt buf "subq $8, %s" (string_of_register v2);
  read_from_address ctx buf (RegisterValue v1) (RegisterValue arg1) 0;
  read_from_address ctx buf (RegisterValue v2) (RegisterValue arg2) 0;
  (* skip if recursive comparison mode *)
  branch_by_value_type ctx buf Eq (RegisterValue num_tmp_reg) compare_label;
  make_marked_int buf (RegisterValue arg1);
  make_marked_int buf (RegisterValue arg2);
  (* compare arg1 and arg2 *)
  start_label buf compare_label;
  let res =
    safe_call ctx buf (string_of_label label) [RegisterValue arg1; RegisterValue arg2]
  in
  free_register count ctx;
  free_register v1 ctx;
  free_register v2 ctx;
  free_register num_tmp_reg ctx;
  branch_if_falsy ctx buf (RegisterValue res) ret_label;
  B.emit_inst_fmt buf "jmp %s" (string_of_label loop_label);
  (* direct comparison branch *)
  start_label buf direct_label;
  let s = comparison_to_value ctx buf Eq (RegisterValue arg1) (RegisterValue arg2) in
  assign_to_register buf s ret_register;
  free1 ctx;
  free2 ctx
;;

let append_string ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* read the two element of closure tuple *)
  let lhs = alloc_stack ctx in
  let rhs = alloc_stack ctx in
  read_from_address ctx buf (RegisterValue a1) (StackValue lhs) (-8);
  read_from_address ctx buf (RegisterValue a1) (StackValue rhs) (-16);
  free1 ctx;
  let lhs_len = alloc_register ctx in
  let rhs_len = alloc_register ctx in
  let size = alloc_stack ctx in
  let len = alloc_stack ctx in
  read_from_address ctx buf (StackValue lhs) (RegisterValue lhs_len) (-8);
  read_from_address ctx buf (StackValue rhs) (RegisterValue rhs_len) (-8);
  restore_marked_int buf (RegisterValue lhs_len);
  restore_marked_int buf (RegisterValue rhs_len);
  assign_to_stack ctx buf (RegisterValue rhs_len) len;
  B.emit_inst_fmt buf "addq %s, %s" (string_of_register lhs_len) (string_of_stack len);
  (* calculate aligned size *)
  assign_to_stack ctx buf (StackValue len) size;
  B.emit_inst_fmt buf "shrq $3, %s" (string_of_stack size);
  B.emit_inst_fmt buf "incq %s" (string_of_stack size);
  B.emit_inst_fmt buf "shlq $3, %s" (string_of_stack size);
  (* add 16 (metadata) *)
  B.emit_inst_fmt buf "addq $16, %s" (string_of_stack size);
  let ptr = alloc_register ctx in
  alloc_heap_ptr ctx buf (StackValue size) (RegisterValue ptr);
  let ptr_save = turn_into_stack ctx buf (RegisterValue ptr) in
  B.emit_inst_fmt buf "subq $8, %s" (string_of_stack size);
  let size_tmp = assign_to_new_register ctx buf (StackValue size) in
  B.emit_inst_fmt buf "shlq $1, %s" (string_of_register size_tmp);
  B.emit_inst_fmt buf "incq %s" (string_of_register size_tmp);
  (* data size *)
  assign_to_address ctx buf (RegisterValue size_tmp) (RegisterValue ptr) 0;
  free_register size_tmp ctx;
  (* string length *)
  make_marked_int buf (StackValue len);
  assign_to_address ctx buf (StackValue len) (RegisterValue ptr) (-8);
  (* copy strings *)
  let src_tmp = assign_to_new_register ctx buf (StackValue lhs) in
  string_value_to_content ctx buf (RegisterValue src_tmp) (RegisterValue src_tmp);
  B.emit_inst_fmt buf "subq %s, %s" (string_of_stack size) (string_of_register ptr);
  let _ =
    safe_call
      ctx
      buf
      "memcpy@PLT"
      [RegisterValue ptr; RegisterValue src_tmp; RegisterValue lhs_len]
  in
  assign_to_register buf (StackValue rhs) src_tmp;
  string_value_to_content ctx buf (RegisterValue src_tmp) (RegisterValue src_tmp);
  B.emit_inst_fmt buf "addq %s, %s" (string_of_register lhs_len) (string_of_register ptr);
  (* incr to include one byte of null *)
  B.emit_inst_fmt buf "incq %s" (string_of_register rhs_len);
  let _ =
    safe_call
      ctx
      buf
      "memcpy@PLT"
      [RegisterValue ptr; RegisterValue src_tmp; RegisterValue rhs_len]
  in
  free_register src_tmp ctx;
  free_register lhs_len ctx;
  free_register rhs_len ctx;
  assign_to_register buf (StackValue ptr_save) ret_register
;;

let length_string ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* read the length of string *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue ret_register) (-8);
  free1 ctx
;;

let shallow_copy ctx buf _label _ret_label =
  let src = nth_arg_stack ctx buf 0 in
  let dest = alloc_register ctx in
  let size = alloc_register ctx in
  (* read data size *)
  read_from_address ctx buf (StackValue src) (RegisterValue size) 0;
  restore_marked_int buf (RegisterValue size);
  alloc_heap_ptr ctx buf (RegisterValue size) (RegisterValue dest);
  let ptr = push_to_stack ctx buf (RegisterValue dest) in
  B.emit_inst_fmt buf "subq %s, %s" (string_of_register size) (string_of_register dest);
  B.emit_inst_fmt buf "subq %s, %s" (string_of_register size) (string_of_stack src);
  let _ =
    safe_call
      ctx
      buf
      "memcpy@PLT"
      [RegisterValue dest; StackValue src; RegisterValue size]
  in
  free_register size ctx;
  assign_to_register buf (StackValue ptr) ret_register;
  free_register dest ctx
;;

let identity ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* just return it *)
  assign_to_register buf (RegisterValue a1) ret_register;
  free1 ctx
;;

let get_string ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* read the two element of closure tuple *)
  let lhs = alloc_register ctx in
  let rhs = alloc_register ctx in
  read_from_address ctx buf (RegisterValue a1) (RegisterValue lhs) (-8);
  read_from_address ctx buf (RegisterValue a1) (RegisterValue rhs) (-16);
  free1 ctx;
  (* function body *)
  restore_marked_int buf (RegisterValue rhs);
  (* assume lhs holds pointer to a string *)
  string_value_to_content ctx buf (RegisterValue lhs) (RegisterValue lhs);
  B.emit_inst_fmt buf "addq %s, %s" (string_of_register rhs) (string_of_register lhs);
  free_register rhs ctx;
  (* take one byte (one character) *)
  B.emit_inst_fmt
    buf
    "movzbq (%s), %s"
    (string_of_register lhs)
    (string_of_register lhs);
  make_marked_int buf (RegisterValue lhs);
  assign_to_register buf (RegisterValue lhs) ret_register;
  free_register lhs ctx

let set_string ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* read the two element of closure tuple *)
  let str = alloc_register ctx in
  let idx = alloc_register ctx in
  let chr = alloc_register ctx in
  read_from_address ctx buf (RegisterValue a1) (RegisterValue str) (-8);
  read_from_address ctx buf (RegisterValue a1) (RegisterValue idx) (-16);
  read_from_address ctx buf (RegisterValue a1) (RegisterValue chr) (-24);
  free1 ctx;
  (* function body *)
  restore_marked_int buf (RegisterValue idx);
  (* assume str holds pointer to a string *)
  string_value_to_content ctx buf (RegisterValue str) (RegisterValue str);
  B.emit_inst_fmt buf "addq %s, %s" (string_of_register idx) (string_of_register str);
  free_register idx ctx;
  (* take one byte (one character) *)
  (* Use rdx temporarily (8-bit register(dl) is needed) *)
  let rdx = Register "%rdx" in
  use_register ctx rdx;
  assign_to_register buf (RegisterValue chr) rdx;
  B.emit_inst_fmt
    buf
    "movb %%dl, (%s)"
    (string_of_register str);
  free_register rdx ctx;
  free_register str ctx;
  free_register chr ctx

let runtimes =
  [ match_fail, match_fail_name
  ; print_int, "print_int"
  ; print_char, "print_char"
  ; print_string, "print_string"
  ; equal, "equal"
  ; append_string, "append_string"
  ; length_string, "length_string"
  ; get_string, "get_string"
  ; set_string, "set_string"
  ; shallow_copy, "shallow_copy"
  ; identity, "identity" ]
;;

let emit_all f =
  let aux (emitter, name) = f name emitter in
  List.iter aux runtimes
;;
