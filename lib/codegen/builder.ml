module P = Parser
module Pat = Tree.Pattern
module B = Output_buffer

type register = Register of string
type stack = Stack of int
type label = Label of string

type value =
  | StackValue of stack
  | RegisterValue of register
  | ConstantValue of int

let stack_value s = StackValue s
let register_value r = RegisterValue r
let constat_value c = RegisterValue c
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
  ; mutable fields : (string, int) Hashtbl.t
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
let make_name_of_runtime = Printf.sprintf "_mlml_%s"
let match_fail_name = "match_fail"
let match_fail_label = Label (make_name_of_runtime match_fail_name)
let argv_label = Label (make_name_of_runtime "argv")

let new_local_env () =
  {unused_registers = usable_registers; current_stack = -8; vars = Hashtbl.create 10}
;;

let new_context () =
  { used_labels = LS.empty
  ; ctors = Hashtbl.create 32
  ; fields = Hashtbl.create 32
  ; current_env = new_local_env () }
;;

let use_env ctx env =
  let old_env = ctx.current_env in
  ctx.current_env <- env;
  old_env
;;

let escape_label_name name =
  let aux c = match c with 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' -> c | _ -> '_' in
  String.map aux name
;;

let new_label ctx name =
  let name = escape_label_name name in
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

let make_marked_int buf v =
  (* TODO: Use imul or add? *)
  B.emit_inst_fmt buf "shlq $1, %s" (string_of_value v);
  B.emit_inst_fmt buf "incq %s" (string_of_value v)
;;

let calc_marked_const i = (i * 2) + 1
let make_marked_const i = ConstantValue (calc_marked_const i)
let restore_marked_int buf v = B.emit_inst_fmt buf "shrq $1, %s" (string_of_value v)
let start_label buf label = B.emit buf (B.Label (string_of_label label))

let start_global_label buf label =
  B.emit_inst buf @@ ".globl " ^ string_of_label label;
  start_label buf label
;;

let assign_to_register buf v reg =
  B.emit_inst_fmt buf "movq %s, %s" (string_of_value v) (string_of_register reg)
;;

let turn_into_register ctx buf = function
  | RegisterValue r -> r, fun _ -> ()
  | v ->
    let new_register = alloc_register ctx in
    assign_to_register buf v new_register;
    new_register, free_register new_register
;;

let assign_to_new_register ctx buf v =
  let r = alloc_register ctx in
  assign_to_register buf v r;
  r
;;

let rec assign_to_stack ctx buf v stack =
  match v with
  | RegisterValue _ | ConstantValue _ ->
    B.emit_inst_fmt buf "movq %s, %s" (string_of_value v) (string_of_stack stack)
  | StackValue _ ->
    let reg, free = turn_into_register ctx buf v in
    assign_to_stack ctx buf (RegisterValue reg) stack;
    free ctx
;;

let alloc_stack ctx =
  let c = ctx.current_env.current_stack in
  let s = Stack c in
  (ctx.current_env).current_stack <- c - 8;
  s
;;

let push_to_stack ctx buf v =
  let s = alloc_stack ctx in
  assign_to_stack ctx buf v s;
  s
;;

let turn_into_stack ctx buf = function StackValue s -> s | v -> push_to_stack ctx buf v

let assign_to_value ctx buf a b =
  match a, b with
  | StackValue _, StackValue _ ->
    let reg = assign_to_new_register ctx buf a in
    B.emit_inst_fmt buf "movq %s, %s" (string_of_register reg) (string_of_value b);
    free_register reg ctx
  | _ -> B.emit_inst_fmt buf "movq %s, %s" (string_of_value a) (string_of_value b)
;;

let assign_to_address ctx buf src dest offset =
  let src, free_src = turn_into_register ctx buf src in
  let dest, free_dest = turn_into_register ctx buf dest in
  B.emit_inst_fmt
    buf
    "movq %s, %d(%s)"
    (string_of_register src)
    offset
    (string_of_register dest);
  free_src ctx;
  free_dest ctx
;;

let read_from_address ctx buf src dest_raw offset =
  let src, free_src = turn_into_register ctx buf src in
  let dest, free_dest = turn_into_register ctx buf dest_raw in
  B.emit_inst_fmt
    buf
    "movq %d(%s), %s"
    offset
    (string_of_register src)
    (string_of_register dest);
  free_src ctx;
  B.emit_inst_fmt buf "movq %s, %s" (string_of_register dest) (string_of_value dest_raw);
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
  B.emit_inst buf @@ "call " ^ name;
  List.iter (fun f -> f ctx) free_fns;
  let restore (x, s) = assign_to_register buf (StackValue s) x in
  List.iter restore saved_regs;
  ret_register
;;

let call_runtime ctx buf name =
  let real_name = make_name_of_runtime name in
  if LS.mem (Label real_name) ctx.used_labels
  then safe_call ctx buf real_name
  else
    failwith @@ Printf.sprintf "could not find a runtime function named '%s'" real_name
;;

let define_ctor ctx ctor idx = Hashtbl.add ctx.ctors ctor idx

let get_ctor_index ctx ctor =
  match Hashtbl.find_opt ctx.ctors ctor with
  | Some i -> i
  | None -> failwith @@ Printf.sprintf "could not find ctor named %s" ctor
;;

let define_field ctx field idx = Hashtbl.add ctx.fields field idx

let get_field_index ctx field =
  match Hashtbl.find_opt ctx.fields field with
  | Some i -> i
  | None -> failwith @@ Printf.sprintf "could not find field named %s" field
;;

let define_variable ctx buf ident v =
  (* TODO: Print warning when ident is accidentally "_" *)
  let s = turn_into_stack ctx buf v in
  Hashtbl.add ctx.current_env.vars ident s
;;

let undef_variable ctx ident = Hashtbl.remove ctx.current_env.vars ident

let get_variable ctx ident =
  match Hashtbl.find_opt ctx.current_env.vars ident with
  | Some s -> s
  | None -> failwith @@ Printf.sprintf "could not find variable named %s" ident
;;

let label_ptr_to_register buf label reg =
  B.emit_inst_fmt
    buf
    "leaq %s(%%rip), %s"
    (string_of_label label)
    (string_of_register reg)
;;

let alloc_heap_ptr_raw ctx buf size dest =
  let ptr = RegisterValue (safe_call ctx buf "malloc@PLT" [size]) in
  B.emit_inst_fmt buf "addq %s, %s" (string_of_value size) (string_of_value ptr);
  B.emit_inst_fmt buf "subq $8, %s" (string_of_value ptr);
  match dest with
  | RegisterValue r -> assign_to_register buf ptr r
  | StackValue s -> assign_to_stack ctx buf ptr s
  | ConstantValue _ -> failwith "can't assign to constant"
;;

let alloc_heap_ptr ctx buf size dest =
  let reg = assign_to_new_register ctx buf size in
  alloc_heap_ptr_raw ctx buf (RegisterValue reg) dest;
  free_register reg ctx
;;

let alloc_heap_ptr_constsize ctx buf size dest =
  alloc_heap_ptr_raw ctx buf (ConstantValue size) dest
;;

(* calculate aligned size *)
let calc_aligned_size buf size =
  B.emit_inst_fmt buf "shrq $3, %s" (string_of_value size);
  B.emit_inst_fmt buf "incq %s" (string_of_value size);
  B.emit_inst_fmt buf "shlq $3, %s" (string_of_value size)
;;

let calc_aligned_size_const size = ((size / 8) + 1) * 8

let make_tuple_const ctx buf values =
  let size = List.length values in
  let reg = alloc_register ctx in
  let reg_value = RegisterValue reg in
  alloc_heap_ptr_constsize ctx buf ((size + 1) * 8) reg_value;
  assign_to_address ctx buf (ConstantValue (size * 8 * 2)) reg_value 0;
  List.iteri (fun i x -> assign_to_address ctx buf x reg_value (-(i + 1) * 8)) values;
  let s = turn_into_stack ctx buf reg_value in
  free_register reg ctx;
  StackValue s
;;

let call_runtime_mlml ctx buf name params =
  let params = match params with [v] -> v | l -> make_tuple_const ctx buf l in
  let cls = make_tuple_const ctx buf [params; make_tuple_const ctx buf []] in
  call_runtime ctx buf name [cls]
;;

let undef_variable_pattern ctx pat =
  List.iter (undef_variable ctx) (Pat.introduced_ident_list pat)
;;

let function_ptr ctx buf label =
  let reg = alloc_register ctx in
  label_ptr_to_register buf label reg;
  let s = StackValue (turn_into_stack ctx buf (RegisterValue reg)) in
  free_register reg ctx;
  s
;;

type comparison =
  | Eq
  | Ne
  | Gt
  | Ge
  | Lt
  | Le

let string_of_comparison = function
  | Eq -> "e"
  | Ne -> "ne"
  | Gt -> "g"
  | Ge -> "ge"
  | Lt -> "l"
  | Le -> "le"
;;

let branch_by_comparison ctx buf cmp v1 v2 label =
  let value, free = turn_into_register ctx buf v2 in
  B.emit_inst_fmt buf "cmpq %s, %s" (string_of_value v1) (string_of_register value);
  free ctx;
  B.emit_inst_fmt buf "j%s %s" (string_of_comparison cmp) (string_of_label label)
;;

let branch_by_value ctx buf cmp = branch_by_comparison ctx buf cmp (make_marked_const 0)
let branch_if_falsy ctx buf = branch_by_value ctx buf Eq
let branch_if_truthy ctx buf = branch_by_value ctx buf Ne

let branch_by_value_type ctx buf cmp value label =
  let value, free = turn_into_register ctx buf value in
  (* If the value is pointer, ZF is set to 1 *)
  (* otherwise, ZF is set to 0               *)
  B.emit_inst_fmt buf "test $1, %s" (string_of_register value);
  free ctx;
  B.emit_inst_fmt buf "j%s %s" (string_of_comparison cmp) (string_of_label label)
;;

let branch_if_pointer ctx buf = branch_by_value_type ctx buf Eq
let branch_if_not_pointer ctx buf = branch_by_value_type ctx buf Ne

let comparison_to_value ctx buf cmp v1 v2 =
  let v2, free = turn_into_register ctx buf v2 in
  (* Use rdx temporarily (8-bit register(dl) is needed) *)
  let rdx = Register "%rdx" in
  use_register ctx rdx;
  B.emit_inst_fmt buf "cmpq %s, %s" (string_of_value v1) (string_of_register v2);
  free ctx;
  B.emit_inst_fmt buf "set%s %%dl" (string_of_comparison cmp);
  B.emit_inst buf "movzbq %dl, %rdx";
  make_marked_int buf (RegisterValue rdx);
  let s = push_to_stack ctx buf (RegisterValue rdx) in
  free_register rdx ctx;
  StackValue s
;;

let string_value_to_content ctx buf v dest =
  let reg = alloc_register ctx in
  (* read the size of data *)
  read_from_address ctx buf v (RegisterValue reg) 0;
  restore_marked_int buf (RegisterValue reg);
  assign_to_value ctx buf v dest;
  B.emit_inst_fmt buf "subq %s, %s" (string_of_register reg) (string_of_value dest);
  free_register reg ctx
;;

let shallow_copy ctx buf src dest =
  let ret = call_runtime_mlml ctx buf "shallow_copy" [src] in
  assign_to_value ctx buf (RegisterValue ret) dest
;;

let make_string_const ctx buf s =
  let len = String.length s in
  let aligned = calc_aligned_size_const len in
  (* emit data *)
  let str_label = new_unnamed_label ctx in
  let pad = aligned - len - 1 in
  B.emit_sub_inst_fmt buf ".string \"%s\"" @@ String.escaped s;
  B.emit_sub_inst_fmt buf ".fill %d" pad;
  B.emit_sub_inst_fmt buf ".quad %d" @@ calc_marked_const len;
  B.emit_sub buf (B.Label (string_of_label str_label));
  B.emit_sub_inst_fmt buf ".quad %d" @@ calc_marked_const (aligned + 8);
  let r = alloc_register ctx in
  label_ptr_to_register buf str_label r;
  let res = StackValue (alloc_stack ctx) in
  shallow_copy ctx buf (RegisterValue r) res;
  free_register r ctx;
  res
;;

let rec pattern_match ctx buf pat v fail_label =
  match pat with
  | Pat.Wildcard -> ()
  | Pat.Var x -> define_variable ctx buf x v
  | Pat.Array values | Pat.Tuple values ->
    (* assume v holds heap address *)
    (* length match *)
    (* size in bytes ( *8 ), recursive data ( *2 ) *)
    let len = List.length values * 8 * 2 in
    let reg = alloc_register ctx in
    read_from_address ctx buf v (RegisterValue reg) 0;
    branch_by_comparison ctx buf Ne (RegisterValue reg) (ConstantValue len) fail_label;
    free_register reg ctx;
    (* content match *)
    let aux i p =
      let reg = alloc_register ctx in
      let reg_value = RegisterValue reg in
      read_from_address ctx buf v reg_value (-(i + 1) * 8);
      let s = turn_into_stack ctx buf reg_value in
      free_register reg ctx;
      pattern_match ctx buf p (StackValue s) fail_label
    in
    List.iteri aux values
  | Pat.Record fields ->
    (* assume v holds heap address *)
    let aux (name, p) =
      let i = get_field_index ctx name in
      let reg = alloc_register ctx in
      let reg_value = RegisterValue reg in
      read_from_address ctx buf v reg_value (-(i + 1) * 8);
      let s = turn_into_stack ctx buf reg_value in
      free_register reg ctx;
      pattern_match ctx buf p (StackValue s) fail_label
    in
    List.iter aux fields
  | Pat.Ctor (name, p) ->
    (* assume v holds heap address *)
    let actual_idx = get_ctor_index ctx name in
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    read_from_address ctx buf v reg_value (-8);
    restore_marked_int buf reg_value;
    B.emit_inst_fmt buf "cmpq $%d, %s" actual_idx (string_of_register reg);
    B.emit_inst_fmt buf "jne %s" (string_of_label fail_label);
    (match p with
    | Some p ->
      read_from_address ctx buf v reg_value (-16);
      let s = turn_into_stack ctx buf reg_value in
      free_register reg ctx;
      pattern_match ctx buf p (StackValue s) fail_label
    | None -> free_register reg ctx)
  | Pat.Int x ->
    let reg, free = turn_into_register ctx buf v in
    restore_marked_int buf (RegisterValue reg);
    B.emit_inst_fmt buf "cmpq $%d, %s" x (string_of_register reg);
    B.emit_inst_fmt buf "jne %s" (string_of_label fail_label);
    free ctx
  | Pat.String s ->
    let sv = make_string_const ctx buf s in
    let ret = call_runtime ctx buf "equal" [v; sv] in
    branch_if_falsy ctx buf (RegisterValue ret) fail_label
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
    B.emit_inst_fmt buf "jmp %s" (string_of_label join_label);
    start_label buf right_label;
    pattern_match ctx buf b v fail_label;
    List.iter2 store_result idents resulting_area;
    start_label buf join_label;
    let redef_vars name s = define_variable ctx buf name (StackValue s) in
    List.iter2 redef_vars idents resulting_area
  | Pat.Cons (a, b) ->
    (* assume v holds heap address *)
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    (* read the flag *)
    read_from_address ctx buf v reg_value (-8);
    restore_marked_int buf reg_value;
    (* nil -> 0, cons -> 1 *)
    B.emit_inst_fmt buf "cmpq $%d, %s" 1 (string_of_register reg);
    B.emit_inst_fmt buf "jne %s" (string_of_label fail_label);
    read_from_address ctx buf v reg_value (-16);
    let s1 = turn_into_stack ctx buf reg_value in
    read_from_address ctx buf v reg_value (-24);
    let s2 = turn_into_stack ctx buf reg_value in
    free_register reg ctx;
    pattern_match ctx buf a (StackValue s1) fail_label;
    pattern_match ctx buf b (StackValue s2) fail_label
  | Pat.Nil ->
    (* assume v holds heap address *)
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    (* read the flag *)
    read_from_address ctx buf v reg_value (-8);
    restore_marked_int buf reg_value;
    (* nil -> 0, cons -> 1 *)
    B.emit_inst_fmt buf "cmpq $%d, %s" 0 (string_of_register reg);
    B.emit_inst_fmt buf "jne %s" (string_of_label fail_label);
    free_register reg ctx
  | Pat.Range (from, to_) ->
    let reg = assign_to_new_register ctx buf v in
    let rv = RegisterValue reg in
    restore_marked_int buf rv;
    branch_by_comparison ctx buf Lt (ConstantValue (Char.code from)) rv fail_label;
    branch_by_comparison ctx buf Gt (ConstantValue (Char.code to_)) rv fail_label;
    free_register reg ctx
;;

let calc_div ctx buf lhs rhs quot rem =
  let rax = Register "%rax" in
  assign_to_register buf (RegisterValue lhs) rax;
  B.emit_inst buf "cltd";
  B.emit_inst_fmt buf "idivq %s" (string_of_register rhs);
  let rdx = Register "%rdx" in
  match quot with
  | Some quot -> assign_to_value ctx buf (RegisterValue rax) quot
  | None ->
    ();
    (match rem with
    | Some rem -> assign_to_value ctx buf (RegisterValue rdx) rem
    | None -> ())
;;
