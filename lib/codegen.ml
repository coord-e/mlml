module P = Parser
module Pat = Pattern
module Expr = Expression
module Def = Definition
module Item = Module_item

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

(* function-local environment *)
(* TODO: current_stack is path-local,
 * consider an alternative way to allocate stack area *)
type local_env =
  { mutable current_stack : int
  ; mutable vars : (string, stack) Hashtbl.t }

type context =
  { mutable unused_registers : register list
  ; mutable used_labels : label list
  ; mutable current_env : local_env }

let usable_registers =
  [ Register "%rsi"
  ; Register "%r8"
  ; Register "%r9"
  ; Register "%r10"
  ; Register "%r11"
  ; Register "%rdx" ]
;;

let ret_register = Register "%rax"
let print_int_label = Label "_print_int"
let new_local_env () = {current_stack = -8; vars = Hashtbl.create 10}

let new_context () =
  { unused_registers = usable_registers
  ; used_labels = [print_int_label]
  ; current_env = new_local_env () }
;;

let use_env ctx env =
  let old_env = ctx.current_env in
  ctx.current_env <- env;
  old_env
;;

let new_label ctx name =
  let is_used label = List.mem label ctx.used_labels in
  let use_label label =
    ctx.used_labels <- label :: ctx.used_labels;
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
  if List.mem reg ctx.unused_registers
  then ctx.unused_registers <- List.filter (fun x -> x != reg) ctx.unused_registers
  else failwith @@ Printf.sprintf "Register '%s' is unavailable" (string_of_register reg)
;;

let alloc_register context =
  match context.unused_registers with
  | h :: t ->
    context.unused_registers <- t;
    h
  | [] -> failwith "Could not allocate register"
;;

let free_register reg ctx =
  if not (List.mem reg ctx.unused_registers)
  then ctx.unused_registers <- reg :: ctx.unused_registers
;;

let emit_instruction buf inst =
  Buffer.add_char buf '\t';
  Buffer.add_string buf inst;
  Buffer.add_char buf '\n'
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

let push_to_stack ctx buf v =
  emit_instruction buf @@ Printf.sprintf "pushq %s" (string_of_value v);
  let c = ctx.current_env.current_stack in
  (ctx.current_env).current_stack <- c - 8;
  Stack c
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
  if List.mem r usable_registers
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

let call_ext_func ctx buf name args =
  (* save registers (used but not by arguments) *)
  (* usable - unused - args                     *)
  let aux i v =
    let reg, free = nth_arg_register ctx i in
    assign_to_register buf v reg;
    reg, free
  in
  let arg_regs, free_fns = List.mapi aux args |> List.split in
  let filt x = not (List.mem x ctx.unused_registers || List.mem x arg_regs) in
  let regs_to_save = List.filter filt usable_registers in
  let saver x =
    let s = push_to_stack ctx buf (RegisterValue x) in
    x, s
  in
  let saved_regs = List.map saver regs_to_save in
  emit_instruction buf @@ "call " ^ name;
  List.iter (fun f -> f ctx) free_fns;
  let restore (x, s) = assign_to_register buf (StackValue s) x in
  List.iter restore saved_regs;
  ret_register
;;

let alloc_heap_ptr ctx buf size dest =
  let ptr = RegisterValue (call_ext_func ctx buf "GC_malloc@PLT" [size]) in
  match dest with
  | RegisterValue r -> assign_to_register buf ptr r
  | StackValue s -> assign_to_stack ctx buf ptr s
  | ConstantValue _ -> failwith "can't assign to constant"
;;

let define_variable ctx buf ident v =
  let s = turn_into_stack ctx buf v in
  Hashtbl.add ctx.current_env.vars ident s
;;

let undef_variable ctx ident = Hashtbl.remove ctx.current_env.vars ident
let get_variable ctx ident = Hashtbl.find ctx.current_env.vars ident

let rec define_variable_pattern ctx buf pat v =
  match pat with
  | Pat.Var x -> define_variable ctx buf x v
  | Pat.Tuple values ->
    (* assume v holds heap address *)
    let aux i p =
      let reg = alloc_register ctx in
      let reg_value = RegisterValue reg in
      read_from_address ctx buf v reg_value (-i * 8);
      let s = turn_into_stack ctx buf reg_value in
      free_register reg ctx;
      define_variable_pattern ctx buf p (StackValue s)
    in
    List.iteri aux values
;;

let undef_variable_pattern ctx pat =
  List.iter (undef_variable ctx) (Pat.introduced_idents pat)
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

let rec codegen_expr ctx buf = function
  | Expr.Int num -> ConstantValue num
  | Expr.Add (lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    emit_instruction buf
    @@ Printf.sprintf "addq %s, %s" (string_of_value lhs) (string_of_register rhs);
    let s = turn_into_stack ctx buf (RegisterValue rhs) in
    free ctx;
    StackValue s
  | Expr.Sub (lhs, rhs) ->
    let rhs = codegen_expr ctx buf rhs in
    let lhs, free = codegen_expr ctx buf lhs |> turn_into_register ctx buf in
    emit_instruction buf
    @@ Printf.sprintf "subq %s, %s" (string_of_value rhs) (string_of_register lhs);
    let s = turn_into_stack ctx buf (RegisterValue lhs) in
    free ctx;
    StackValue s
  | Expr.Mul (lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    emit_instruction buf
    @@ Printf.sprintf "imulq %s, %s" (string_of_value lhs) (string_of_register rhs);
    let s = turn_into_stack ctx buf (RegisterValue rhs) in
    free ctx;
    StackValue s
  | Expr.LetVar (pat, lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    define_variable_pattern ctx buf pat lhs;
    let rhs = codegen_expr ctx buf rhs in
    undef_variable_pattern ctx pat;
    rhs
  | Expr.Var ident ->
    (match ident with
    | "print_int" -> function_ptr ctx buf print_int_label
    | _ -> StackValue (get_variable ctx ident))
  | Expr.LetFun (is_rec, ident, params, lhs, rhs) ->
    let lhs = emit_function_value ctx buf is_rec ident params lhs in
    define_variable ctx buf ident lhs;
    let rhs = codegen_expr ctx buf rhs in
    undef_variable ctx ident;
    rhs
  | Expr.App (lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    let param, free = nth_arg_register ctx 0 in
    assign_to_register buf rhs param;
    emit_instruction buf @@ Printf.sprintf "call *%s" (string_of_value lhs);
    free ctx;
    StackValue (turn_into_stack ctx buf (RegisterValue ret_register))
  | Expr.IfThenElse (cond, then_, else_) ->
    let cond, free = codegen_expr ctx buf cond |> turn_into_register ctx buf in
    let eval_stack = push_to_stack ctx buf (ConstantValue 0) in
    emit_instruction buf @@ Printf.sprintf "cmpq $0, %s" (string_of_register cond);
    free ctx;
    let then_label = new_unnamed_label ctx in
    emit_instruction buf @@ Printf.sprintf "jne %s" (string_of_label then_label);
    let join_label = new_unnamed_label ctx in
    let save_stack_c = ctx.current_env.current_stack in
    let else_ = codegen_expr ctx buf else_ in
    assign_to_stack ctx buf else_ eval_stack;
    emit_instruction buf @@ Printf.sprintf "jmp %s" (string_of_label join_label);
    start_label buf then_label;
    (ctx.current_env).current_stack <- save_stack_c;
    let then_ = codegen_expr ctx buf then_ in
    assign_to_stack ctx buf then_ eval_stack;
    start_label buf join_label;
    StackValue eval_stack
  | Expr.Equal (lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    (* Use rdx temporarily (8-bit register(dl) is needed) *)
    let rdx = Register "%rdx" in
    use_register ctx rdx;
    emit_instruction buf
    @@ Printf.sprintf "cmpq %s, %s" (string_of_value lhs) (string_of_register rhs);
    free ctx;
    emit_instruction buf "sete %dl";
    emit_instruction buf "movzbq %dl, %rdx";
    let s = push_to_stack ctx buf (RegisterValue rdx) in
    free_register rdx ctx;
    StackValue s
  | Expr.Tuple values ->
    let size = List.length values in
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    alloc_heap_ptr ctx buf (ConstantValue (size * 2)) reg_value;
    let values = List.map (codegen_expr ctx buf) values in
    List.iteri (fun i x -> assign_to_address ctx buf x reg_value (-i * 8)) values;
    let s = StackValue (turn_into_stack ctx buf reg_value) in
    free_register reg ctx;
    s

and codegen_definition ctx buf = function
  | Def.LetVar (pat, lhs) ->
    let lhs = codegen_expr ctx buf lhs in
    define_variable_pattern ctx buf pat lhs
  | Def.LetFun (is_rec, ident, params, lhs) ->
    let lhs = emit_function_value ctx buf is_rec ident params lhs in
    define_variable ctx buf ident lhs

and codegen_module_item ctx buf = function
  | Item.Definition def -> codegen_definition ctx buf def
  | Item.Expression expr ->
    let _ = codegen_expr ctx buf expr in
    ()

and codegen_module ctx buf = List.iter (codegen_module_item ctx buf)

and emit_function_with ctx main_buf name fn =
  let old_env = use_env ctx @@ new_local_env () in
  let buf = Buffer.create 100 in
  let label = new_label ctx name in
  start_global_label buf label;
  emit_instruction buf "pushq %rbp";
  emit_instruction buf "movq %rsp, %rbp";
  fn ctx buf label;
  emit_instruction buf "movq %rbp, %rsp";
  emit_instruction buf "popq %rbp";
  emit_instruction buf "ret";
  let _ = use_env ctx old_env in
  (* TODO: Use more effective and sufficient way to prepend to the buffer *)
  Buffer.add_buffer buf main_buf;
  Buffer.reset main_buf;
  Buffer.add_buffer main_buf buf;
  label

and emit_function ctx main_buf is_rec name params ast =
  let emit ctx buf label =
    List.iteri
      (fun i pat ->
        let arg = nth_arg_stack ctx buf i in
        define_variable_pattern ctx buf pat (StackValue arg) )
      params;
    (if is_rec
    then
      let ptr = function_ptr ctx buf label in
      define_variable ctx buf name ptr);
    let value = codegen_expr ctx buf ast in
    assign_to_register buf value ret_register
  in
  emit_function_with ctx main_buf name emit

and emit_function_value ctx buf is_rec name params ast =
  let label = emit_function ctx buf is_rec name params ast in
  function_ptr ctx buf label

and emit_module ctx buf name items =
  let emit ctx buf _label =
    (* TODO: more generic and explicit method *)
    if name = "main" then emit_instruction buf "call GC_init@PLT";
    codegen_module ctx buf items
  in
  emit_function_with ctx buf name emit
;;

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
  movq	%rdi, %rsi
  leaq	.string_of_print_int(%rip), %rdi
  movl	$0, %eax
  call	printf@PLT
  leave
  ret
|}
;;

let f ast =
  let buf = Buffer.create 100 in
  let ctx = new_context () in
  let label = emit_module ctx buf "main" ast in
  assert (string_of_label label = "main");
  emit_print_int_function buf;
  Buffer.contents buf
;;
