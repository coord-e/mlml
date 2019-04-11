module P = Parser
module Pat = P.Pattern

type register = Register of string
type stack = Stack of int
type label = Label of string

type value =
  | StackValue of stack
  | RegisterValue of register
  | ConstantValue of int

let string_of_register = function Register n -> n
let string_of_stack = function Stack num -> string_of_int num ^ "(%rbp)"
let string_of_label = function Label n -> n
let string_of_constant num = "$" ^ string_of_int num

let string_of_value = function
  | StackValue num -> string_of_stack num
  | RegisterValue kind -> string_of_register kind
  | ConstantValue num -> string_of_constant num
;;

module RS = Set.Make (struct
  type t = register

  let compare = compare
end)

module LS = Set.Make (struct
  type t = label

  let compare = compare
end)

(* function-local environment *)
type local_env =
  { mutable unused_registers : RS.t
  ; mutable current_stack : int
  ; mutable vars : (string, stack) Hashtbl.t }

type context =
  { mutable used_labels : LS.t
  ; mutable ctors : (string, int) Hashtbl.t
  ; mutable current_env : local_env }

let usable_registers =
  RS.of_list
    [ Register "%rsi"
    ; Register "%r8"
    ; Register "%r9"
    ; Register "%r10"
    ; Register "%r11"
    ; Register "%rdx" ]
;;

(* https://wiki.osdev.org/System_V_ABI#x86-64 *)
let volatile_registers =
  RS.of_list
    [ Register "%rax"
    ; Register "%rdi"
    ; Register "%rsi"
    ; Register "%rdx"
    ; Register "%rcx"
    ; Register "%r8"
    ; Register "%r9"
    ; Register "%r10"
    ; Register "%r11" ]
;;

let non_volatile_registers =
  RS.of_list
    [ Register "%rbx"
    ; Register "%rsp"
    ; Register "%rbp"
    ; Register "%r12"
    ; Register "%r13"
    ; Register "%r14"
    ; Register "%r15" ]
;;

let ret_register = Register "%rax"
let print_int_label = Label "_print_int"

let new_local_env () =
  {unused_registers = usable_registers; current_stack = -8; vars = Hashtbl.create 10}
;;

let emit_match_fail buf =
  Buffer.add_string
    buf
    {|
.section .rodata
.string_of_match_fail:
  .string	"runtime error: patten match failed. aborted."
.match_fail:
  leaq	.string_of_match_fail(%rip), %rdi
  call	puts@PLT
  movl	$1, %eax
  call	exit@PLT
|}
;;

let match_fail_label = Label ".match_fail"

let emit_print_int_function buf =
  Buffer.add_string
    buf
    {|
.section .rodata
.string_of_print_int:
  .string	"%ld"
_print_int:
  pushq	%rbp
  movq	%rsp, %rbp
  movq	-8(%rdi), %rsi
  shrq $1, %rsi
  leaq	.string_of_print_int(%rip), %rdi
  movl	$0, %eax
  call	printf@PLT
  leave
  ret
|}
;;

let new_context () =
  { used_labels = LS.of_list [print_int_label; match_fail_label]
  ; ctors = Hashtbl.create 32
  ; current_env = new_local_env () }
;;

let use_env ctx env =
  let old_env = ctx.current_env in
  ctx.current_env <- env;
  old_env
;;

let new_label ctx name =
  let is_used label = LS.mem label ctx.used_labels in
  let use_label label =
    ctx.used_labels <- LS.add label ctx.used_labels;
    label
  in
  let rec aux i =
    let label = Label (Printf.sprintf "%s%d" name i) in
    if is_used label then aux (i + 1) else use_label label
  in
  let raw = Label name in
  if is_used raw then aux 1 else use_label raw
;;

let new_unnamed_label ctx = new_label ctx ".L"

let use_register ctx reg =
  if RS.mem reg ctx.current_env.unused_registers
  then
    (ctx.current_env).unused_registers
    <- RS.filter (fun x -> x != reg) ctx.current_env.unused_registers
  else failwith @@ Printf.sprintf "Register '%s' is unavailable" (string_of_register reg)
;;

let alloc_register context =
  match RS.choose_opt context.current_env.unused_registers with
  | Some h ->
    (context.current_env).unused_registers
    <- RS.remove h context.current_env.unused_registers;
    h
  | None -> failwith "Could not allocate register"
;;

let free_register reg ctx =
  if not (RS.mem reg ctx.current_env.unused_registers)
  then (ctx.current_env).unused_registers <- RS.add reg ctx.current_env.unused_registers
;;

let emit_instruction buf inst =
  Buffer.add_char buf '\t';
  Buffer.add_string buf inst;
  Buffer.add_char buf '\n'
;;

let make_marked_int buf reg =
  (* TODO: Use imul or add? *)
  emit_instruction buf @@ Printf.sprintf "shlq $1, %s" (string_of_register reg);
  emit_instruction buf @@ Printf.sprintf "incq %s" (string_of_register reg)
;;

let make_marked_const i = ConstantValue ((i * 2) + 1)

let restore_marked_int buf reg =
  emit_instruction buf @@ Printf.sprintf "shrq $1, %s" (string_of_register reg)
;;

let start_label buf label = Buffer.add_string buf @@ string_of_label label ^ ":\n"

let start_global_label buf label =
  emit_instruction buf @@ ".globl " ^ string_of_label label;
  start_label buf label
;;

let assign_to_register buf v reg =
  emit_instruction buf
  @@ Printf.sprintf "movq %s, %s" (string_of_value v) (string_of_register reg)
;;

let turn_into_register ctx buf = function
  | RegisterValue r -> r, fun _ -> ()
  | v ->
    let new_register = alloc_register ctx in
    assign_to_register buf v new_register;
    new_register, free_register new_register
;;

let rec assign_to_stack ctx buf v stack =
  match v with
  | RegisterValue _ | ConstantValue _ ->
    emit_instruction buf
    @@ Printf.sprintf "movq %s, %s" (string_of_value v) (string_of_stack stack)
  | StackValue _ ->
    let reg, free = turn_into_register ctx buf v in
    assign_to_stack ctx buf (RegisterValue reg) stack;
    free ctx
;;

let push_to_stack ctx buf v =
  let c = ctx.current_env.current_stack in
  let s = Stack c in
  assign_to_stack ctx buf v s;
  (ctx.current_env).current_stack <- c - 8;
  s
;;

let turn_into_stack ctx buf = function StackValue s -> s | v -> push_to_stack ctx buf v

let assign_to_address ctx buf src dest offset =
  let src, free_src = turn_into_register ctx buf src in
  let dest, free_dest = turn_into_register ctx buf dest in
  emit_instruction buf
  @@ Printf.sprintf
       "movq %s, %d(%s)"
       (string_of_register src)
       offset
       (string_of_register dest);
  free_src ctx;
  free_dest ctx
;;

let read_from_address ctx buf src dest offset =
  let src, free_src = turn_into_register ctx buf src in
  let dest, free_dest = turn_into_register ctx buf dest in
  emit_instruction buf
  @@ Printf.sprintf
       "movq %d(%s), %s"
       offset
       (string_of_register src)
       (string_of_register dest);
  free_src ctx;
  free_dest ctx
;;

let nth_arg_register context n =
  let r =
    match n with
    | 0 -> Register "%rdi"
    | 1 -> Register "%rsi"
    | 2 -> Register "%rdx"
    | 3 -> Register "%rcx"
    | 4 -> Register "%r8"
    | 5 -> Register "%r9"
    | _ -> failwith "Too many arguments"
  in
  if RS.mem r usable_registers
  then (
    use_register context r;
    r, free_register r )
  else r, fun _ -> ()
;;

let nth_arg_stack ctx buf n =
  let r, free = nth_arg_register ctx n in
  let s = turn_into_stack ctx buf (RegisterValue r) in
  free ctx;
  s
;;

let safe_call ctx buf name args =
  (* save registers (used but not by arguments) *)
  (* volatile - unused - args - %rax            *)
  let aux i v =
    let reg, free = nth_arg_register ctx i in
    assign_to_register buf v reg;
    reg, free
  in
  let arg_regs, free_fns = List.mapi aux args |> List.split in
  let filt x =
    not
      ( RS.mem x ctx.current_env.unused_registers
      || List.mem x arg_regs
      || x = ret_register )
  in
  let regs_to_save = RS.filter filt volatile_registers in
  let saver x =
    let s = push_to_stack ctx buf (RegisterValue x) in
    x, s
  in
  let saved_regs = RS.elements regs_to_save |> List.map saver in
  emit_instruction buf @@ "call " ^ name;
  List.iter (fun f -> f ctx) free_fns;
  let restore (x, s) = assign_to_register buf (StackValue s) x in
  List.iter restore saved_regs;
  ret_register
;;

let define_ctor ctx ctor idx = Hashtbl.add ctx.ctors ctor idx
let get_ctor_index ctx ctor = Hashtbl.find ctx.ctors ctor

let define_variable ctx buf ident v =
  (* TODO: Print warning when ident is accidentally "_" *)
  let s = turn_into_stack ctx buf v in
  Hashtbl.add ctx.current_env.vars ident s
;;

let undef_variable ctx ident = Hashtbl.remove ctx.current_env.vars ident
let get_variable ctx ident = Hashtbl.find ctx.current_env.vars ident

let rec pattern_match ctx buf pat v fail_label =
  match pat with
  | Pat.Var "_" -> ()
  | Pat.Var x -> define_variable ctx buf x v
  | Pat.Tuple values ->
    (* assume v holds heap address *)
    let aux i p =
      let reg = alloc_register ctx in
      let reg_value = RegisterValue reg in
      read_from_address ctx buf v reg_value (-(i + 1) * 8);
      let s = turn_into_stack ctx buf reg_value in
      free_register reg ctx;
      pattern_match ctx buf p (StackValue s) fail_label
    in
    List.iteri aux values
  | Pat.Ctor (name, p) ->
    (* assume v holds heap address *)
    let actual_idx = get_ctor_index ctx name in
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    read_from_address ctx buf v reg_value (-8);
    emit_instruction buf
    @@ Printf.sprintf "cmpq $%d, %s" actual_idx (string_of_register reg);
    emit_instruction buf @@ Printf.sprintf "jne %s" (string_of_label fail_label);
    (match p with
    | Some p ->
      read_from_address ctx buf v reg_value (-16);
      let s = turn_into_stack ctx buf reg_value in
      free_register reg ctx;
      pattern_match ctx buf p (StackValue s) fail_label
    | None -> free_register reg ctx)
  | Pat.Int x ->
    let reg, free = turn_into_register ctx buf v in
    restore_marked_int buf reg;
    emit_instruction buf @@ Printf.sprintf "cmpq $%d, %s" x (string_of_register reg);
    emit_instruction buf @@ Printf.sprintf "jne %s" (string_of_label fail_label);
    free ctx
  | Pat.Or (a, b) ->
    let idents = Pat.introduced_ident_list a in
    if idents <> Pat.introduced_ident_list b
    then failwith "introduced identifiers mismatch in | pattern";
    let resulting_area =
      List.map (fun _ -> push_to_stack ctx buf (ConstantValue 0)) idents
    in
    let store_result name s =
      (* TODO: `v` can be freed here *)
      let v = get_variable ctx name in
      assign_to_stack ctx buf (StackValue v) s;
      undef_variable ctx name
    in
    let right_label = new_unnamed_label ctx in
    let join_label = new_unnamed_label ctx in
    pattern_match ctx buf a v right_label;
    List.iter2 store_result idents resulting_area;
    emit_instruction buf @@ Printf.sprintf "jmp %s" (string_of_label join_label);
    start_label buf right_label;
    pattern_match ctx buf b v fail_label;
    List.iter2 store_result idents resulting_area;
    start_label buf join_label;
    let redef_vars name s = define_variable ctx buf name (StackValue s) in
    List.iter2 redef_vars idents resulting_area
;;

let undef_variable_pattern ctx pat =
  List.iter (undef_variable ctx) (Pat.introduced_ident_list pat)
;;

let function_ptr_to_register buf label reg =
  emit_instruction buf
  @@ Printf.sprintf "leaq %s(%%rip), %s" (string_of_label label) (string_of_register reg)
;;

let function_ptr ctx buf label =
  let reg = alloc_register ctx in
  function_ptr_to_register buf label reg;
  let s = StackValue (turn_into_stack ctx buf (RegisterValue reg)) in
  free_register reg ctx;
  s
;;

let branch_by_value ctx buf value false_label =
  let value, free = turn_into_register ctx buf value in
  restore_marked_int buf value;
  emit_instruction buf @@ Printf.sprintf "cmpq $0, %s" (string_of_register value);
  free ctx;
  emit_instruction buf @@ Printf.sprintf "je %s" (string_of_label false_label)
;;

let alloc_heap_ptr_raw ctx buf size dest =
  let ptr = RegisterValue (safe_call ctx buf "GC_malloc@PLT" [size]) in
  match dest with
  | RegisterValue r -> assign_to_register buf ptr r
  | StackValue s -> assign_to_stack ctx buf ptr s
  | ConstantValue _ -> failwith "can't assign to constant"
;;

let alloc_heap_ptr ctx buf size dest =
  let reg = alloc_register ctx in
  assign_to_register buf size reg;
  restore_marked_int buf reg;
  alloc_heap_ptr_raw ctx buf (RegisterValue reg) dest
;;

let alloc_heap_ptr_constsize ctx buf size dest =
  alloc_heap_ptr_raw ctx buf (ConstantValue size) dest
;;
