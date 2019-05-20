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

let get_argv ctx buf _label _ret_label =
  let storage_ptr = alloc_register ctx in
  label_ptr_to_register buf argv_label storage_ptr;
  read_from_address ctx buf (RegisterValue storage_ptr) (RegisterValue ret_register) 0
;;

let handle_argv ctx buf _label _ret_label =
  (* emit data *)
  B.emit_sub_inst buf ".data";
  B.emit_sub buf (B.Label (string_of_label argv_label));
  B.emit_sub_inst buf ".fill 8";
  B.emit_sub_inst buf ".section .rodata";
  (* emit function body *)
  let argc = nth_arg_stack ctx buf 0 |> stack_value in
  let argv = nth_arg_stack ctx buf 1 |> stack_value in
  let size_tmp = assign_to_new_register ctx buf argc in
  make_marked_int buf (RegisterValue size_tmp);
  let ptr =
    call_runtime_mlml ctx buf "create_array" [RegisterValue size_tmp]
    |> register_value
    |> assign_to_new_register ctx buf
  in
  free_register size_tmp ctx;
  let ptr_save = push_to_stack ctx buf (RegisterValue ptr) |> stack_value in
  (* loop until count = target *)
  let target = argc in
  let count = assign_to_new_register ctx buf (ConstantValue 0) in
  let loop_label = new_unnamed_label ctx in
  start_label buf loop_label;
  (* loop block *)
  B.emit_inst_fmt buf "subq $8, %s" (string_of_register ptr);
  let argv_tmp = alloc_register ctx in
  read_from_address ctx buf argv (RegisterValue argv_tmp) 0;
  let str =
    call_runtime ctx buf "c_str_to_string" [RegisterValue argv_tmp]
    |> register_value
    |> assign_to_new_register ctx buf
  in
  free_register argv_tmp ctx;
  assign_to_address ctx buf (RegisterValue str) (RegisterValue ptr) 0;
  free_register str ctx;
  B.emit_inst_fmt buf "incq %s" (string_of_register count);
  B.emit_inst_fmt buf "addq $8, %s" (string_of_value argv);
  branch_by_comparison ctx buf Ne target (RegisterValue count) loop_label;
  free_register count ctx;
  free_register ptr ctx;
  (* end of loop *)
  let storage_ptr = alloc_register ctx in
  label_ptr_to_register buf argv_label storage_ptr;
  assign_to_address ctx buf ptr_save (RegisterValue storage_ptr) 0;
  free_register storage_ptr ctx
;;

let c_str_to_string ctx buf _label _ret_label =
  let a1 = nth_arg_stack ctx buf 0 |> stack_value in
  let len =
    safe_call ctx buf "strlen@PLT" [a1]
    |> register_value
    |> assign_to_new_register ctx buf
  in
  let size_tmp = assign_to_new_register ctx buf (RegisterValue len) in
  make_marked_int buf (RegisterValue size_tmp);
  let ptr =
    call_runtime_mlml ctx buf "create_string" [RegisterValue size_tmp]
    |> register_value
    |> assign_to_new_register ctx buf
  in
  free_register size_tmp ctx;
  let ptr_save = push_to_stack ctx buf (RegisterValue ptr) |> stack_value in
  string_value_to_content ctx buf (RegisterValue ptr) (RegisterValue ptr);
  let _ = safe_call ctx buf "memcpy@PLT" [RegisterValue ptr; a1; RegisterValue len] in
  free_register len ctx;
  free_register ptr ctx;
  assign_to_register buf ptr_save ret_register
;;

let print_char ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  restore_marked_int buf (RegisterValue a1);
  let _ = safe_call ctx buf "putchar@PLT" [RegisterValue a1] in
  free1 ctx;
  assign_to_register buf (make_tuple_const ctx buf []) ret_register
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
  free2 ctx;
  assign_to_register buf (make_tuple_const ctx buf []) ret_register
;;

let prerr_string ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  let a2, free2 = nth_arg_register ctx 1 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* assume reg is a pointer to string value *)
  string_value_to_content ctx buf (RegisterValue a1) (RegisterValue a1);
  B.emit_inst_fmt buf "movq stderr(%%rip), %s" (string_of_register a2);
  let _ = safe_call ctx buf "fputs@PLT" [RegisterValue a1; RegisterValue a2] in
  free1 ctx;
  free2 ctx;
  assign_to_register buf (make_tuple_const ctx buf []) ret_register
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
  calc_aligned_size buf (StackValue size);
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
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  let src = alloc_stack ctx in
  read_from_address ctx buf (RegisterValue a1) (StackValue src) (-8);
  free1 ctx;
  let dest = alloc_register ctx in
  let size = alloc_register ctx in
  (* read data size *)
  read_from_address ctx buf (StackValue src) (RegisterValue size) 0;
  restore_marked_int buf (RegisterValue size);
  alloc_heap_ptr ctx buf (RegisterValue size) (RegisterValue dest);
  let ptr = push_to_stack ctx buf (RegisterValue dest) in
  B.emit_inst_fmt buf "subq %s, %s" (string_of_register size) (string_of_register dest);
  B.emit_inst_fmt buf "subq %s, %s" (string_of_register size) (string_of_stack src);
  B.emit_inst_fmt buf "addq $8, %s" (string_of_register size);
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
  (* read the two element of tuple *)
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
  B.emit_inst_fmt buf "movzbq (%s), %s" (string_of_register lhs) (string_of_register lhs);
  make_marked_int buf (RegisterValue lhs);
  assign_to_register buf (RegisterValue lhs) ret_register;
  free_register lhs ctx
;;

let set_string ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* read the two element of tuple *)
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
  restore_marked_int buf (RegisterValue chr);
  (* Use rdx temporarily (8-bit register(dl) is needed) *)
  let rdx = Register "rdx" in
  use_register ctx rdx;
  assign_to_register buf (RegisterValue chr) rdx;
  B.emit_inst_fmt buf "movb %%dl, (%s)" (string_of_register str);
  free_register rdx ctx;
  free_register str ctx;
  free_register chr ctx
;;

let create_string ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  restore_marked_int buf (RegisterValue a1);
  (* actual length of string *)
  let len = alloc_register ctx in
  (* allocated size *)
  let size = alloc_register ctx in
  assign_to_register buf (RegisterValue a1) len;
  assign_to_register buf (RegisterValue a1) size;
  free1 ctx;
  calc_aligned_size buf (RegisterValue size);
  (* add 16 (metadata) *)
  B.emit_inst_fmt buf "addq $16, %s" (string_of_register size);
  let ptr = alloc_register ctx in
  alloc_heap_ptr ctx buf (RegisterValue size) (RegisterValue ptr);
  (* data size *)
  B.emit_inst_fmt buf "subq $8, %s" (string_of_register size);
  make_marked_int buf (RegisterValue size);
  assign_to_address ctx buf (RegisterValue size) (RegisterValue ptr) 0;
  make_marked_int buf (RegisterValue len);
  assign_to_address ctx buf (RegisterValue len) (RegisterValue ptr) (-8);
  free_register size ctx;
  free_register len ctx;
  assign_to_register buf (RegisterValue ptr) ret_register;
  free_register ptr ctx
;;

let get_array ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* read the two element of tuple *)
  let lhs = alloc_register ctx in
  let rhs = alloc_register ctx in
  read_from_address ctx buf (RegisterValue a1) (RegisterValue lhs) (-8);
  read_from_address ctx buf (RegisterValue a1) (RegisterValue rhs) (-16);
  free1 ctx;
  (* function body *)
  restore_marked_int buf (RegisterValue rhs);
  B.emit_inst_fmt buf "incq %s" (string_of_register rhs);
  B.emit_inst_fmt buf "shlq $3, %s" (string_of_register rhs);
  B.emit_inst_fmt buf "subq %s, %s" (string_of_register rhs) (string_of_register lhs);
  free_register rhs ctx;
  let reg = alloc_register ctx in
  read_from_address ctx buf (RegisterValue lhs) (RegisterValue reg) 0;
  free_register lhs ctx;
  assign_to_register buf (RegisterValue reg) ret_register;
  free_register reg ctx
;;

let set_array ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* read the three element of tuple *)
  let ary = alloc_register ctx in
  let idx = alloc_register ctx in
  let v = alloc_register ctx in
  read_from_address ctx buf (RegisterValue a1) (RegisterValue ary) (-8);
  read_from_address ctx buf (RegisterValue a1) (RegisterValue idx) (-16);
  read_from_address ctx buf (RegisterValue a1) (RegisterValue v) (-24);
  free1 ctx;
  (* function body *)
  restore_marked_int buf (RegisterValue idx);
  B.emit_inst_fmt buf "incq %s" (string_of_register idx);
  B.emit_inst_fmt buf "shlq $3, %s" (string_of_register idx);
  B.emit_inst_fmt buf "subq %s, %s" (string_of_register idx) (string_of_register ary);
  free_register idx ctx;
  assign_to_address ctx buf (RegisterValue v) (RegisterValue ary) 0;
  free_register ary ctx;
  free_register v ctx;
  assign_to_register buf (make_tuple_const ctx buf []) ret_register
;;

let length_array ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  let reg = alloc_register ctx in
  (* read metadata *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue reg) 0;
  free1 ctx;
  restore_marked_int buf (RegisterValue reg);
  B.emit_inst_fmt buf "shrq $3, %s" (string_of_register reg);
  make_marked_int buf (RegisterValue reg);
  assign_to_register buf (RegisterValue reg) ret_register;
  free_register reg ctx
;;

let create_array ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  restore_marked_int buf (RegisterValue a1);
  let len = assign_to_new_register ctx buf (RegisterValue a1) in
  let size = assign_to_new_register ctx buf (RegisterValue a1) in
  let ptr = alloc_register ctx in
  free1 ctx;
  (* calc stored size *)
  (* (len + 1) * 8 *)
  B.emit_inst_fmt buf "incq %s" (string_of_register size);
  B.emit_inst_fmt buf "shlq $3, %s" (string_of_register size);
  alloc_heap_ptr ctx buf (RegisterValue size) (RegisterValue ptr);
  free_register size ctx;
  (* calc size *)
  (* len * 8 * 2 *)
  B.emit_inst_fmt buf "shlq $4, %s" (string_of_register len);
  assign_to_address ctx buf (RegisterValue len) (RegisterValue ptr) 0;
  free_register len ctx;
  assign_to_register buf (RegisterValue ptr) ret_register;
  free_register ptr ctx
;;

let exit ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  restore_marked_int buf (RegisterValue a1);
  let _ = safe_call ctx buf "exit@PLT" [RegisterValue a1] in
  free1 ctx
;;

let file_exists ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* assume reg is a pointer to string value *)
  string_value_to_content ctx buf (RegisterValue a1) (RegisterValue a1);
  let v =
    (* 2nd arguments is `F_OK` *)
    safe_call ctx buf "access@PLT" [RegisterValue a1; ConstantValue 0]
    |> register_value
    |> comparison_to_value ctx buf Eq (ConstantValue 0)
  in
  assign_to_register buf v ret_register;
  free1 ctx
;;

let is_directory ctx buf _label ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* assume reg is a pointer to string value *)
  string_value_to_content ctx buf (RegisterValue a1) (RegisterValue a1);
  let res =
    safe_call ctx buf "opendir@PLT" [RegisterValue a1]
    |> register_value
    |> assign_to_new_register ctx buf
  in
  free1 ctx;
  (* return with rax = false *)
  assign_to_register buf (make_marked_const 0) ret_register;
  branch_by_comparison ctx buf Eq (ConstantValue 0) (RegisterValue res) ret_label;
  (* if found, close it and assign rax = true *)
  let _ = safe_call ctx buf "closedir@PLT" [RegisterValue res] in
  free_register res ctx;
  assign_to_register buf (make_marked_const 1) ret_register
;;

let getcwd ctx buf _label _ret_label =
  (* get_current_dir_name(3) is GNU-dependent, but I don't care *)
  (* memory leak happens here, but I don't care                 *)
  let res = safe_call ctx buf "get_current_dir_name@PLT" [] in
  let _ = call_runtime ctx buf "c_str_to_string" [RegisterValue res] in
  ()
;;

let readdir_filter_name = "readdir_filter"
let readdir_filter_label = Label (make_name_of_runtime readdir_filter_name)

let readdir_filter ctx buf _label ret_label =
  (* use from scandir(3) in readdir runtime              *)
  (* name[0] != '.' || (name[1] != 0 && name[1] != '.'); *)
  let a1, free1 = nth_arg_register ctx 0 in
  (* 19 is a magic number (d_name) *)
  B.emit_inst_fmt buf "addq $19, %s" (string_of_register a1);
  let dot_char = ConstantValue (Char.code '.') in
  let tmp = assign_to_new_register ctx buf (RegisterValue a1) in
  B.emit_inst_fmt buf "movzbq (%s), %s" (string_of_register tmp) (string_of_register tmp);
  (* return with rax = true if 1st char is not dot *)
  assign_to_register buf (ConstantValue 1) ret_register;
  branch_by_comparison ctx buf Ne dot_char (RegisterValue tmp) ret_label;
  assign_to_register buf (RegisterValue a1) tmp;
  free1 ctx;
  B.emit_inst_fmt
    buf
    "movzbq 1(%s), %s"
    (string_of_register tmp)
    (string_of_register tmp);
  (* return with rax = false if 2nd char is dot ".." *)
  assign_to_register buf (ConstantValue 0) ret_register;
  branch_by_comparison ctx buf Eq dot_char (RegisterValue tmp) ret_label;
  (* return with rax = false if 2nd char is null "." *)
  branch_by_comparison ctx buf Eq (ConstantValue 0) (RegisterValue tmp) ret_label;
  assign_to_register buf (ConstantValue 1) ret_register;
  free_register tmp ctx
;;

let readdir ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* assume reg is a pointer to string value *)
  string_value_to_content ctx buf (RegisterValue a1) (RegisterValue a1);
  (* where namelist is stored *)
  let dest = alloc_stack ctx |> stack_value in
  (* retrieve all directory entries *)
  (* call scandir(3) *)
  let dest_addr = alloc_register ctx in
  let sort_addr = alloc_register ctx in
  let filt_addr = alloc_register ctx in
  B.emit_inst_fmt buf "leaq %s, %s" (string_of_value dest) (string_of_register dest_addr);
  label_ptr_to_register buf readdir_filter_label filt_addr;
  label_ptr_to_register buf (Label "alphasort@PLT") sort_addr;
  let num_entries =
    safe_call
      ctx
      buf
      "scandir@PLT"
      [ RegisterValue a1
      ; RegisterValue dest_addr
      ; RegisterValue filt_addr
      ; RegisterValue sort_addr ]
    |> register_value
    |> turn_into_stack ctx buf
    |> stack_value
  in
  free1 ctx;
  free_register dest_addr ctx;
  free_register sort_addr ctx;
  free_register filt_addr ctx;
  (* allocate destination array *)
  let size_tmp = assign_to_new_register ctx buf num_entries in
  make_marked_int buf (RegisterValue size_tmp);
  let ary_ptr =
    call_runtime_mlml ctx buf "create_array" [RegisterValue size_tmp]
    |> register_value
    |> assign_to_new_register ctx buf
  in
  free_register size_tmp ctx;
  let ary_ptr_save = turn_into_stack ctx buf (RegisterValue ary_ptr) |> stack_value in
  (* copy namelist to array *)
  (* loop until count = target *)
  let target = num_entries in
  let count = assign_to_new_register ctx buf (ConstantValue 0) in
  let loop_label = new_unnamed_label ctx in
  start_label buf loop_label;
  (* loop block *)
  B.emit_inst_fmt buf "subq $8, %s" (string_of_register ary_ptr);
  let dest_tmp = alloc_register ctx in
  (* 19 is a magic number (d_name) *)
  read_from_address ctx buf dest (RegisterValue dest_tmp) 0;
  B.emit_inst_fmt buf "addq $19, %s" (string_of_register dest_tmp);
  let str =
    call_runtime ctx buf "c_str_to_string" [RegisterValue dest_tmp]
    |> register_value
    |> assign_to_new_register ctx buf
  in
  free_register dest_tmp ctx;
  assign_to_address ctx buf (RegisterValue str) (RegisterValue ary_ptr) 0;
  free_register str ctx;
  B.emit_inst_fmt buf "incq %s" (string_of_register count);
  B.emit_inst_fmt buf "addq $8, %s" (string_of_value dest);
  branch_by_comparison ctx buf Ne target (RegisterValue count) loop_label;
  free_register count ctx;
  free_register ary_ptr ctx;
  (* end of loop *)
  assign_to_register buf ary_ptr_save ret_register
;;

let has_env ctx buf _label ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* assume reg is a pointer to string value *)
  string_value_to_content ctx buf (RegisterValue a1) (RegisterValue a1);
  let res =
    safe_call ctx buf "getenv@PLT" [RegisterValue a1]
    |> register_value
    |> assign_to_new_register ctx buf
  in
  free1 ctx;
  (* return with rax = false if env is not found (getenv(s) = NULL) *)
  assign_to_register buf (make_marked_const 0) ret_register;
  branch_by_comparison ctx buf Eq (ConstantValue 0) (RegisterValue res) ret_label;
  (* otherwise rax = true *)
  assign_to_register buf (make_marked_const 1) ret_register;
  free_register res ctx
;;

let getenv ctx buf _label _ret_label =
  let a1, free1 = nth_arg_register ctx 0 in
  (* read the first element of closure tuple *)
  read_from_address ctx buf (RegisterValue a1) (RegisterValue a1) (-8);
  (* assume reg is a pointer to string value *)
  string_value_to_content ctx buf (RegisterValue a1) (RegisterValue a1);
  let res = safe_call ctx buf "getenv@PLT" [RegisterValue a1] in
  free1 ctx;
  (* rax holds the resulting string *)
  let _ = call_runtime ctx buf "c_str_to_string" [RegisterValue res] in
  ()
;;

let runtimes =
  [ match_fail, match_fail_name
  ; print_char, "print_char"
  ; print_string, "print_string"
  ; prerr_string, "prerr_string"
  ; equal, "equal"
  ; append_string, "append_string"
  ; length_string, "length_string"
  ; get_string, "get_string"
  ; set_string, "set_string"
  ; create_string, "create_string"
  ; shallow_copy, "shallow_copy"
  ; identity, "identity"
  ; length_array, "length_array"
  ; create_array, "create_array"
  ; get_array, "get_array"
  ; set_array, "set_array"
  ; exit, "exit"
  ; c_str_to_string, "c_str_to_string"
  ; handle_argv, "handle_argv"
  ; get_argv, "get_argv"
  ; file_exists, "file_exists"
  ; is_directory, "is_directory"
  ; getcwd, "getcwd"
  ; readdir, "readdir"
  ; readdir_filter, readdir_filter_name
  ; has_env, "has_env"
  ; getenv, "getenv" ]
;;

let emit_all f =
  let aux (emitter, name) = f name emitter in
  List.iter aux runtimes
;;
