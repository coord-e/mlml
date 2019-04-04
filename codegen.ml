module P = Parser

type register = RegName of string
type value =
  | Stack of int
  | Register of register
  | Constant of int
  | Add of register * register


let alloc_register = RegName "edx"

let alloc_stack = Stack 1
(* let alloc_stack context =           *)
(*   let c = context.current_stack in  *)
(*   context.current_stack <- (c - 4); *)
(*   Stack c                           *)

let string_of_register = function
  | RegName n -> n

let string_of_stack num = (string_of_int num) ^ "(%rbp)"
let string_of_constant num = "$" ^ (string_of_int num)

let rec value_to_asm = function
  | Stack num -> (string_of_stack num, "")
  | Register kind -> (string_of_register kind, "")
  | Constant num -> (string_of_constant num, "")
  | Add (lhs, rhs) -> (
    let add = Printf.sprintf "addl %s %s" (string_of_register lhs) (string_of_register rhs) in
    let new_stack = alloc_stack in
    let stack_str, _ = value_to_asm new_stack in
    let mov = Printf.sprintf "movl %s %s" (string_of_register rhs) stack_str in
    (stack_str, String.concat "\n" [add; mov])
  )

let turn_into_register v =
  match v with
  | Stack num -> (
    let new_register = alloc_register in
    let load = Printf.sprintf "movl %s %s" (string_of_stack num) (string_of_register new_register) in
    (new_register, load)
  )
  | Register r -> (r, "")
  | Constant c -> (
    let new_register = alloc_register in
    let load = Printf.sprintf "movl %s %s" (string_of_constant c) (string_of_register new_register) in
    (new_register, load)
  )
  | _ -> failwith "Cannot turn value into register"


let rec codegen_expr = function
  | P.Int num -> (Constant num, "")
  | P.Add (lhs, rhs) -> (
      let lhs, lhs_asm = codegen_expr lhs in
      let lhs, lhs_reg_asm = turn_into_register lhs in
      let rhs, rhs_asm = codegen_expr rhs in
      let rhs, rhs_reg_asm = turn_into_register rhs in
      (Add (lhs, rhs), String.concat "\n" [lhs_asm; rhs_asm; lhs_reg_asm; rhs_reg_asm])
  )
  | P.Mul (lhs, rhs) -> (
      let lhs, lhs_asm = codegen_expr lhs in
      let lhs, lhs_reg_asm = turn_into_register lhs in
      let rhs, rhs_asm = codegen_expr rhs in
      let rhs, rhs_reg_asm = turn_into_register rhs in
      (Add (lhs, rhs), String.concat "\n" [lhs_asm; rhs_asm; lhs_reg_asm; rhs_reg_asm])
  )

let codegen ast =
  let buf = Buffer.create 100 in
  Buffer.add_string buf ".text\n";
  Buffer.add_string buf ".globl main\n";
  Buffer.add_string buf "main:\n";
  Buffer.add_string buf "pushq	%rbp\n";
  Buffer.add_string buf "movq	%rsp, %rbp\n";
  let value, asm = codegen_expr ast in
  Buffer.add_string buf @@ asm ^ "\n";
  let value, asm = value_to_asm value in
  Buffer.add_string buf @@ asm ^ "\n";
  Buffer.add_string buf @@ Printf.sprintf "movl %s, %%eax\n" value;
  Buffer.add_string buf "popq	%rbp\n";
  Buffer.add_string buf "ret\n";
  Buffer.contents buf
