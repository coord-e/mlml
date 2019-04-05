module P = Parser

type register = RegName of string
type value =
  | Stack of int
  | Register of register
  | Constant of int
  | Add of register * register
  | Mul of register * register

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

let rec value_to_asm ctx buf = function
  | Stack num -> string_of_stack num
  | Register kind -> string_of_register kind
  | Constant num -> string_of_constant num
  | Add (lhs, rhs) -> (
    emit_instruction buf @@ Printf.sprintf "addl %s, %s" (string_of_register lhs) (string_of_register rhs);
    let new_stack = alloc_stack ctx |> value_to_asm ctx buf in
    emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_register rhs) new_stack;
    new_stack
  )
  | Mul (lhs, rhs) -> (
    emit_instruction buf @@ Printf.sprintf "imull %s, %s" (string_of_register lhs) (string_of_register rhs);
    let new_stack = alloc_stack ctx |> value_to_asm ctx buf in
    emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_register rhs) new_stack;
    new_stack
  )

let turn_into_register ctx buf = function
  | Stack num -> (
    let new_register = alloc_register ctx in
    emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_stack num) (string_of_register new_register);
    new_register
  )
  | Register r -> r
  | Constant c -> (
    let new_register = alloc_register ctx in
    emit_instruction buf @@ Printf.sprintf "movl %s, %s" (string_of_constant c) (string_of_register new_register);
    new_register
  )
  | _ -> failwith "Cannot turn value into register"


let rec codegen_expr ctx buf = function
  | P.Int num -> Constant num
  | P.Add (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs |> turn_into_register ctx buf in
      let rhs = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
      Add (lhs, rhs)
  )
  | P.Mul (lhs, rhs) -> (
      let lhs = codegen_expr ctx buf lhs |> turn_into_register ctx buf in
      let rhs = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
      Mul (lhs, rhs)
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
  let value = codegen_expr ctx buf ast in
  let str = value_to_asm ctx buf value in
  emit_instruction buf @@ Printf.sprintf "movl %s, %%eax" str;
  emit_instruction buf "popq	%rbp";
  emit_instruction buf "ret";
  Buffer.contents buf
