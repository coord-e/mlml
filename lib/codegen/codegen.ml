open Builder
module P = Parser
module Expr = P.Expression
module Def = P.Definition
module Item = P.Module_item

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
  | Expr.Follow (lhs, rhs) ->
    let _ = codegen_expr ctx buf lhs in
    codegen_expr ctx buf rhs
  | Expr.LetVar (pat, lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    pattern_match ctx buf pat lhs match_fail_label;
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
    let else_ = codegen_expr ctx buf else_ in
    assign_to_stack ctx buf else_ eval_stack;
    emit_instruction buf @@ Printf.sprintf "jmp %s" (string_of_label join_label);
    start_label buf then_label;
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
  | Expr.Ctor (name, value) ->
    let value =
      match value with
      | Some value -> codegen_expr ctx buf value
      (* TODO: Better representation of ctor without parameters *)
      | None -> ConstantValue 0
    in
    let idx = get_ctor_index ctx name in
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    (* two 64-bit values -> 16 *)
    alloc_heap_ptr ctx buf (ConstantValue 16) reg_value;
    assign_to_address ctx buf (ConstantValue idx) reg_value 0;
    assign_to_address ctx buf value reg_value (-8);
    let s = StackValue (turn_into_stack ctx buf reg_value) in
    free_register reg ctx;
    s
  | Expr.Match (v, arms) ->
    let v = codegen_expr ctx buf v in
    let join_label = new_unnamed_label ctx in
    let eval_stack = push_to_stack ctx buf (ConstantValue 0) in
    let save_stack_c = ctx.current_env.current_stack in
    let rec aux = function
      | (pat, when_, rhs) :: t ->
        (ctx.current_env).current_stack <- save_stack_c;
        let next_label = new_unnamed_label ctx in
        (match when_ with
        | Some cond ->
          let cond = codegen_expr ctx buf cond in
          branch_by_value ctx buf cond next_label
        | None -> ());
        pattern_match ctx buf pat v next_label;
        let rhs = codegen_expr ctx buf rhs in
        assign_to_stack ctx buf rhs eval_stack;
        emit_instruction buf @@ Printf.sprintf "jmp %s" (string_of_label join_label);
        start_label buf next_label;
        aux t
      | [] ->
        emit_instruction buf
        @@ Printf.sprintf "jmp %s" (string_of_label match_fail_label);
        start_label buf join_label;
        StackValue eval_stack
    in
    aux arms

and codegen_definition ctx buf = function
  | Def.LetVar (pat, lhs) ->
    let lhs = codegen_expr ctx buf lhs in
    pattern_match ctx buf pat lhs match_fail_label
  | Def.LetFun (is_rec, ident, params, lhs) ->
    let lhs = emit_function_value ctx buf is_rec ident params lhs in
    define_variable ctx buf ident lhs
  | Def.Variant (_, variants) ->
    let aux i (ctor, _) = define_ctor ctx ctor i in
    List.iteri aux variants

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
  (* TODO: more generic and explicit method *)
  if name = "main" then emit_instruction buf "call GC_init@PLT";
  emit_instruction buf @@ Printf.sprintf "$replace_with_subq_%s" (string_of_label label);
  fn ctx buf label;
  let stack_used = ctx.current_env.current_stack in
  emit_instruction buf "movq %rbp, %rsp";
  emit_instruction buf "popq %rbp";
  emit_instruction buf "ret";
  let _ = use_env ctx old_env in
  (* TODO: Use more effective and sufficient way to prepend to the buffer *)
  Buffer.add_buffer buf main_buf;
  Buffer.reset main_buf;
  (* TODO: Use more effective way to insert subq instruction *)
  let replace x =
    let s = Printf.sprintf "replace_with_subq_%s" (string_of_label label) in
    if x = s then Printf.sprintf "subq $%d, %%rsp" (-stack_used + 8) else "$" ^ x
  in
  Buffer.add_substitute main_buf replace (Buffer.contents buf);
  label

and emit_function ctx main_buf is_rec name params ast =
  let emit ctx buf label =
    List.iteri
      (fun i pat ->
        let arg = nth_arg_stack ctx buf i in
        pattern_match ctx buf pat (StackValue arg) match_fail_label )
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
    codegen_module ctx buf items;
    assign_to_register buf (ConstantValue 0) ret_register
  in
  emit_function_with ctx buf name emit
;;

let f ast =
  let buf = Buffer.create 100 in
  let ctx = new_context () in
  let label = emit_module ctx buf "main" ast in
  assert (string_of_label label = "main");
  emit_print_int_function buf;
  emit_match_fail buf;
  Buffer.contents buf
;;
