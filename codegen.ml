module P = Parser

type register = RegName of string
type value =
  | Stack of int
  | Register of register
  | Constant of int

type context = {
  mutable current_stack : int;
  mutable unused_registers : register list;
  mutable env : (string, int) Hashtbl.t;
}

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
  context.current_stack <- (c - 4);
  c

let emit_instruction buf inst =
  Buffer.add_string buf inst;
  Buffer.add_char buf '\n'

let string_of_register = function
  | RegName n -> n

let string_of_stack num = (string_of_int num) ^ "(%rbp)"
let string_of_constant num = "$" ^ (string_of_int num)

let value_to_asm = function
  | Stack num -> string_of_stack num
  | Register kind -> string_of_register kind
  | Constant num -> string_of_constant num

let turn_into_register ctx buf = function
  | Stack num -> (
    let new_register = alloc_register ctx in
    emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_stack num) (string_of_register new_register);
    (new_register, free_register new_register)
  )
  | Register r -> (r, fun _ -> ())
  | Constant c -> (
    let new_register = alloc_register ctx in
    emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_constant c) (string_of_register new_register);
    (new_register, free_register new_register)
  )

let turn_into_stack ctx buf = function
  | Stack num -> num
  | Register r -> (
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_register r) (string_of_stack new_stack);
      new_stack
  )
  | Constant c -> (
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_constant c) (string_of_stack new_stack);
      new_stack
  )

let define_variable ctx buf ident v =
  let s = turn_into_stack ctx buf v in
  Hashtbl.add ctx.env ident s

let undef_variable ctx ident =
  Hashtbl.remove ctx.env ident

let get_variable ctx ident =
  Hashtbl.find ctx.env ident

let rec codegen_expr ctx buf = function
  | P.Int num -> Constant num
  | P.Add (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs in
      let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
      emit_instruction buf @@ Printf.sprintf "addl %s, %s" (value_to_asm lhs) (string_of_register rhs);
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_register rhs) (string_of_stack new_stack);
      free ctx;
      Stack new_stack
  )
  | P.Mul (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs in
      let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
      emit_instruction buf @@ Printf.sprintf "imull %s, %s" (value_to_asm lhs) (string_of_register rhs);
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_register rhs) (string_of_stack new_stack);
      free ctx;
      Stack new_stack
  )
  | P.LetVar (ident, lhs, rhs) -> (
    let lhs = codegen_expr ctx buf lhs in
    define_variable ctx buf ident lhs;
    let rhs = codegen_expr ctx buf rhs in
    undef_variable ctx ident;
    rhs
  )
  | P.Var ident -> Stack (get_variable ctx ident)

let codegen ast =
  let ctx = {
    current_stack = -8;
    unused_registers = [RegName "%eax"; RegName "%ebx"; RegName "%ecx"; RegName "%edx"];
    env = Hashtbl.create 10;
  } in
  let buf = Buffer.create 100 in
  emit_instruction buf ".text";
  emit_instruction buf ".globl main";
  emit_instruction buf "main:";
  emit_instruction buf "pushq	%rbp";
  emit_instruction buf "movq	%rsp, %rbp";
  let value = codegen_expr ctx buf ast |> value_to_asm in
  emit_instruction buf @@ Printf.sprintf "movl %s, %%eax" value;
  emit_instruction buf "popq	%rbp";
  emit_instruction buf "ret";
  Buffer.contents buf
