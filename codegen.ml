module P = Parser

type register = Register of string
type stack = Stack of int
type value =
  | StackValue of stack
  | RegisterValue of register
  | ConstantValue of int

let string_of_register = function
  | Register n -> n

let string_of_stack = function
  | Stack num -> (string_of_int num) ^ "(%rbp)"

let string_of_constant num = "$" ^ (string_of_int num)

let string_of_value = function
  | StackValue num -> string_of_stack num
  | RegisterValue kind -> string_of_register kind
  | ConstantValue num -> string_of_constant num

type context = {
  mutable current_stack : int;
  mutable unused_registers : register list;
  mutable env : (string, stack) Hashtbl.t;
}

let usable_registers = [Register "%rsi"; Register "%rdi"; Register "%r8"; Register "%r9"; Register "%r10"; Register "%r11"]
let ret_register = Register "%rax"

let new_context () = {
  current_stack = -8;
  unused_registers = usable_registers;
  env = Hashtbl.create 10;
}

let use_register ctx reg =
  if List.mem reg ctx.unused_registers
  then (
    ctx.unused_registers <-
      List.filter (fun x -> x != reg) ctx.unused_registers
  )
  else failwith @@ Printf.sprintf "Register '%s' is unavailable" (string_of_register reg)

let alloc_register context =
  match context.unused_registers with
  | h :: t -> (
    context.unused_registers <- t;
    h
  )
  | [] -> failwith "Could not allocate register"

let free_register reg context =
  context.unused_registers <- reg :: context.unused_registers

let alloc_stack context =
  let c = context.current_stack in
  context.current_stack <- (c - 8);
  Stack c

let emit_instruction buf inst =
  Buffer.add_string buf inst;
  Buffer.add_char buf '\n'

let turn_into_register ctx buf = function
  | StackValue num -> (
    let new_register = alloc_register ctx in
    emit_instruction buf @@ Printf.sprintf "movq %s, %s" (string_of_stack num) (string_of_register new_register);
    (new_register, free_register new_register)
  )
  | RegisterValue r -> (r, fun _ -> ())
  | ConstantValue c -> (
    let new_register = alloc_register ctx in
    emit_instruction buf @@ Printf.sprintf "movq %s, %s" (string_of_constant c) (string_of_register new_register);
    (new_register, free_register new_register)
  )

let turn_into_stack ctx buf = function
  | StackValue num -> num
  | RegisterValue r -> (
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movq %s, %s" (string_of_register r) (string_of_stack new_stack);
      new_stack
  )
  | ConstantValue c -> (
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movq %s, %s" (string_of_constant c) (string_of_stack new_stack);
      new_stack
  )

let nth_arg_register context n =
  let r = (
    match n with
    | 0 -> Register "%rdi"
    | 1 -> Register "%rsi"
    | 2 -> Register "%rdx"
    | 3 -> Register "%rcx"
    | 4 -> Register "%r8"
    | 5 -> Register "%r9"
    | _ -> failwith "Too many arguments"
  ) in
  if List.mem r usable_registers
  then (
    use_register context r;
    (r, free_register r)
  ) else (r, fun _ -> ())

let nth_arg_stack ctx buf n =
  let r, free = nth_arg_register ctx n in
  let s = turn_into_stack ctx buf (RegisterValue r) in
  free ctx;
  s

let define_variable ctx buf ident v =
  let s = turn_into_stack ctx buf v in
  Hashtbl.add ctx.env ident s

let undef_variable ctx ident =
  Hashtbl.remove ctx.env ident

let get_variable ctx ident =
  Hashtbl.find ctx.env ident

let rec codegen_expr ctx buf = function
  | P.Int num -> ConstantValue num
  | P.Add (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs in
      let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
      emit_instruction buf @@ Printf.sprintf "addq %s, %s" (string_of_value lhs) (string_of_register rhs);
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movq %s, %s" (string_of_register rhs) (string_of_stack new_stack);
      free ctx;
      StackValue new_stack
  )
  | P.Mul (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs in
      let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
      emit_instruction buf @@ Printf.sprintf "imulq %s, %s" (string_of_value lhs) (string_of_register rhs);
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movq %s, %s" (string_of_register rhs) (string_of_stack new_stack);
      free ctx;
      StackValue new_stack
  )
  | P.LetVar (ident, lhs, rhs) -> (
    let lhs = codegen_expr ctx buf lhs in
    define_variable ctx buf ident lhs;
    let rhs = codegen_expr ctx buf rhs in
    undef_variable ctx ident;
    rhs
  )
  | P.Var ident -> StackValue (get_variable ctx ident)
  | P.LetFun (ident, params, lhs, rhs) -> (
    let lhs = emit_function_value ctx buf ident lhs params in
    define_variable ctx buf ident lhs;
    let rhs = codegen_expr ctx buf rhs in
    undef_variable ctx ident;
    rhs
  )
  | P.App (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs in
      let rhs = codegen_expr ctx buf rhs in
      let param, free = nth_arg_register ctx 0 in
      emit_instruction buf @@ Printf.sprintf "movq %s, %s" (string_of_value rhs) (string_of_register param);
      emit_instruction buf @@ Printf.sprintf "call *%s" (string_of_value lhs);
      free ctx;
      RegisterValue ret_register
  )

and emit_function main_buf name ast params =
  let ctx = new_context () in
  let buf = Buffer.create 100 in
  emit_instruction buf @@ ".globl " ^ name;
  emit_instruction buf @@ name ^ ":";
  emit_instruction buf "pushq	%rbp";
  emit_instruction buf "movq	%rsp, %rbp";
  List.iteri (fun i name ->
    let arg = nth_arg_stack ctx buf i in
    define_variable ctx buf name (StackValue arg)
  ) params;
  let value = codegen_expr ctx buf ast |> string_of_value in
  emit_instruction buf @@ Printf.sprintf "movq %s, %s" value (string_of_register ret_register);
  emit_instruction buf "popq	%rbp";
  emit_instruction buf "ret";
  (* TODO: Use more effective and sufficient way to prepend to the buffer *)
  Buffer.add_buffer buf main_buf;
  Buffer.reset main_buf;
  Buffer.add_buffer main_buf buf

and emit_function_value ctx buf name ast params =
  emit_function buf name ast params;
  let reg = alloc_register ctx in
  emit_instruction buf @@ Printf.sprintf "leaq %s(%%rip), %s" name (string_of_register reg);
  let s = StackValue (turn_into_stack ctx buf (RegisterValue reg)) in
  free_register reg ctx;
  s

let codegen ast =
  let buf = Buffer.create 100 in
  emit_function buf "main" ast [];
  Buffer.contents buf
