module P = Parser

type register = RegName of string
type value =
  | Stack of int
  | Register of register
  | Constant of int

type context = {
  mutable current_stack : int;
  mutable unused_registers : register list;
}

let alloc_register context =
  match context.unused_registers with
  | h :: t -> (
    context.unused_registers <- t;
    h
  )
  | [] -> failwith "Could not allocate register"

let free_register context reg =
  context.unused_registers <- reg :: context.unused_registers

let alloc_stack context =
  let c = context.current_stack in
  context.current_stack <- (c - 4);
  Stack c

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
    (new_register, free_register ctx)
  )
  | Register r -> (r, fun _ -> ())
  | Constant c -> (
    let new_register = alloc_register ctx in
    emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_constant c) (string_of_register new_register);
    (new_register, free_register ctx)
  )


let rec codegen_expr ctx buf = function
  | P.Int num -> Constant num
  | P.Add (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs in
      let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
      emit_instruction buf @@ Printf.sprintf "addl %s, %s" (value_to_asm lhs) (string_of_register rhs);
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_register rhs) (value_to_asm new_stack);
      free rhs;
      new_stack
  )
  | P.Mul (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs in
      let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
      emit_instruction buf @@ Printf.sprintf "imull %s, %s" (value_to_asm lhs) (string_of_register rhs);
      let new_stack = alloc_stack ctx in
      emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_register rhs) (value_to_asm new_stack);
      free rhs;
      new_stack
  )

let codegen ast =
  let ctx = {
    current_stack = -8;
    unused_registers = [RegName "%eax"; RegName "%ebx"; RegName "%ecx"; RegName "%edx"];
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
