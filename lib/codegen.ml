module P = Parser

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

type context =
  { mutable current_stack : int
  ; mutable unused_registers : register list
  ; mutable env : (string, stack) Hashtbl.t
  ; mutable label_index : int }

let usable_registers =
  [ Register "%rsi"
  ; Register "%rdi"
  ; Register "%r8"
  ; Register "%r9"
  ; Register "%r10"
  ; Register "%r11" ]
;;

let ret_register = Register "%rax"

let new_context () =
  { current_stack = -8
  ; unused_registers = usable_registers
  ; env = Hashtbl.create 10
  ; label_index = 0 }
;;

let new_unnamed_label ctx =
  let i = ctx.label_index in
  ctx.label_index <- i + 1;
  Label (Printf.sprintf ".L%d" i)
;;

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

let free_register reg context =
  context.unused_registers <- reg :: context.unused_registers
;;

let emit_instruction buf inst =
  Buffer.add_string buf inst;
  Buffer.add_char buf '\n'
;;

let assign_to_register buf v reg =
  emit_instruction buf
  @@ Printf.sprintf "movq %s, %s" (string_of_value v) (string_of_register reg)
;;

let assign_to_stack buf v stack =
  emit_instruction buf
  @@ Printf.sprintf "movq %s, %s" (string_of_value v) (string_of_stack stack)
;;

let push_to_stack ctx buf v =
  emit_instruction buf @@ Printf.sprintf "pushq %s" (string_of_value v);
  let c = ctx.current_stack in
  ctx.current_stack <- c - 8;
  Stack c
;;

let turn_into_register ctx buf = function
  | RegisterValue r -> r, fun _ -> ()
  | v ->
    let new_register = alloc_register ctx in
    assign_to_register buf v new_register;
    new_register, free_register new_register
;;

let turn_into_stack ctx buf = function StackValue s -> s | v -> push_to_stack ctx buf v

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

let define_variable ctx buf ident v =
  let s = turn_into_stack ctx buf v in
  Hashtbl.add ctx.env ident s
;;

let undef_variable ctx ident = Hashtbl.remove ctx.env ident
let get_variable ctx ident = Hashtbl.find ctx.env ident

let rec codegen_expr ctx buf = function
  | P.Int num -> ConstantValue num
  | P.Add (lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    emit_instruction buf
    @@ Printf.sprintf "addq %s, %s" (string_of_value lhs) (string_of_register rhs);
    let s = turn_into_stack ctx buf (RegisterValue rhs) in
    free ctx;
    StackValue s
  | P.Mul (lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    emit_instruction buf
    @@ Printf.sprintf "imulq %s, %s" (string_of_value lhs) (string_of_register rhs);
    let s = turn_into_stack ctx buf (RegisterValue rhs) in
    free ctx;
    StackValue s
  | P.LetVar (ident, lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    define_variable ctx buf ident lhs;
    let rhs = codegen_expr ctx buf rhs in
    undef_variable ctx ident;
    rhs
  | P.Var ident -> StackValue (get_variable ctx ident)
  | P.LetFun (ident, params, lhs, rhs) ->
    let lhs = emit_function_value ctx buf ident lhs params in
    define_variable ctx buf ident lhs;
    let rhs = codegen_expr ctx buf rhs in
    undef_variable ctx ident;
    rhs
  | P.App (lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    let param, free = nth_arg_register ctx 0 in
    assign_to_register buf rhs param;
    emit_instruction buf @@ Printf.sprintf "call *%s" (string_of_value lhs);
    free ctx;
    StackValue (turn_into_stack ctx buf (RegisterValue ret_register))
  | P.IfThenElse (cond, then_, else_) ->
    let cond, free = codegen_expr ctx buf cond |> turn_into_register ctx buf in
    emit_instruction buf @@ Printf.sprintf "cmpq $0, %s" (string_of_register cond);
    free ctx;
    let then_label = new_unnamed_label ctx in
    emit_instruction buf @@ Printf.sprintf "jne %s" (string_of_label then_label);
    let eval_stack = push_to_stack ctx buf (ConstantValue 0) in
    let join_label = new_unnamed_label ctx in
    let else_ = codegen_expr ctx buf else_ in
    assign_to_stack buf else_ eval_stack;
    emit_instruction buf @@ Printf.sprintf "jmp %s" (string_of_label join_label);
    emit_instruction buf @@ string_of_label then_label ^ ":";
    let then_ = codegen_expr ctx buf then_ in
    assign_to_stack buf then_ eval_stack;
    emit_instruction buf @@ string_of_label join_label ^ ":";
    StackValue eval_stack

and emit_function main_buf name ast params =
  let ctx = new_context () in
  let buf = Buffer.create 100 in
  emit_instruction buf @@ ".globl " ^ name;
  emit_instruction buf @@ name ^ ":";
  emit_instruction buf "pushq\t%rbp";
  emit_instruction buf "movq\t%rsp, %rbp";
  List.iteri
    (fun i name ->
      let arg = nth_arg_stack ctx buf i in
      define_variable ctx buf name (StackValue arg) )
    params;
  let value = codegen_expr ctx buf ast in
  assign_to_register buf value ret_register;
  emit_instruction buf "movq %rbp, %rsp";
  emit_instruction buf "popq\t%rbp";
  emit_instruction buf "ret";
  (* TODO: Use more effective and sufficient way to prepend to the buffer *)
  Buffer.add_buffer buf main_buf;
  Buffer.reset main_buf;
  Buffer.add_buffer main_buf buf

and emit_function_value ctx buf name ast params =
  emit_function buf name ast params;
  let reg = alloc_register ctx in
  emit_instruction buf
  @@ Printf.sprintf "leaq %s(%%rip), %s" name (string_of_register reg);
  let s = StackValue (turn_into_stack ctx buf (RegisterValue reg)) in
  free_register reg ctx;
  s
;;

let codegen ast =
  let buf = Buffer.create 100 in
  emit_function buf "main" ast [];
  Buffer.contents buf
;;
