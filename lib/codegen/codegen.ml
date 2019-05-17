open Builder
module P = Parser
module Expr = Tree.Expression
module Mod = Tree.Module
module Binop = Tree.Binop
module Uop = Tree.Unaryop
module B = Output_buffer

let rec codegen_binop ctx buf lhs rhs = function
  | Binop.Add ->
    (* make(a) + make(b)     *)
    (* = (2a + 1) + (2b + 1) *)
    (* = 2(a + b) + 2        *)
    (* = make(a + b) + 1     *)
    let lhs = codegen_expr ctx buf lhs in
    let rhs, free = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    B.emit_inst_fmt buf "addq %s, %s" (string_of_value lhs) (string_of_register rhs);
    B.emit_inst_fmt buf "decq %s" (string_of_register rhs);
    let s = turn_into_stack ctx buf (RegisterValue rhs) in
    free ctx;
    StackValue s
  | Binop.Sub ->
    (* make(a) - make(b)     *)
    (* = (2a + 1) - (2b + 1) *)
    (* = 2(a - b) *)
    (* = make(a - b) - 1 *)
    let rhs = codegen_expr ctx buf rhs in
    let lhs, free = codegen_expr ctx buf lhs |> turn_into_register ctx buf in
    B.emit_inst_fmt buf "subq %s, %s" (string_of_value rhs) (string_of_register lhs);
    B.emit_inst_fmt buf "incq %s" (string_of_register lhs);
    let s = turn_into_stack ctx buf (RegisterValue lhs) in
    free ctx;
    StackValue s
  | Binop.Mul ->
    (* make(a*b)                           *)
    (* = 1/2 * (make(a) - 1) * make(b) + 1 *)
    let lhs, free_l = codegen_expr ctx buf lhs |> turn_into_register ctx buf in
    let rhs, free_r = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    B.emit_inst_fmt buf "sarq $1, %s" (string_of_register lhs);
    B.emit_inst_fmt buf "decq %s" (string_of_register rhs);
    B.emit_inst_fmt buf "imulq %s, %s" (string_of_register lhs) (string_of_register rhs);
    free_l ctx;
    B.emit_inst_fmt buf "incq %s" (string_of_register rhs);
    let s = turn_into_stack ctx buf (RegisterValue rhs) in
    free_r ctx;
    StackValue s
  | Binop.Div ->
    let lhs, free_l = codegen_expr ctx buf lhs |> turn_into_register ctx buf in
    let rhs, free_r = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    restore_marked_int buf (RegisterValue lhs);
    restore_marked_int buf (RegisterValue rhs);
    let quot = StackValue (alloc_stack ctx) in
    calc_div ctx buf lhs rhs (Some quot) None;
    free_l ctx;
    free_r ctx;
    make_marked_int buf quot;
    quot
  | Binop.Mod ->
    let lhs, free_l = codegen_expr ctx buf lhs |> turn_into_register ctx buf in
    let rhs, free_r = codegen_expr ctx buf rhs |> turn_into_register ctx buf in
    restore_marked_int buf (RegisterValue lhs);
    restore_marked_int buf (RegisterValue rhs);
    let rem = StackValue (alloc_stack ctx) in
    calc_div ctx buf lhs rhs None (Some rem);
    free_l ctx;
    free_r ctx;
    make_marked_int buf rem;
    rem
  | Binop.Or -> codegen_expr ctx buf (Expr.IfThenElse (lhs, lhs, rhs))
  | Binop.And -> codegen_expr ctx buf (Expr.IfThenElse (lhs, rhs, lhs))
  | Binop.Follow ->
    let _ = codegen_expr ctx buf lhs in
    codegen_expr ctx buf rhs
  | Binop.NotPhysicalEqual ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    comparison_to_value ctx buf Ne lhs rhs
  | Binop.Equal ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    let ret = call_runtime ctx buf "equal" [lhs; rhs] in
    StackValue (turn_into_stack ctx buf (RegisterValue ret))
  | Binop.Lt ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    comparison_to_value ctx buf Gt lhs rhs
  | Binop.Gt ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    comparison_to_value ctx buf Lt lhs rhs
  | Binop.Cons ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    (* size, flag, lhs, rhs -> 8 * 4 *)
    alloc_heap_ptr_constsize ctx buf 32 reg_value;
    (* data size *)
    assign_to_address ctx buf (ConstantValue (24 * 2)) reg_value 0;
    (* nil -> 0, cons -> 1 *)
    assign_to_address ctx buf (make_marked_const 1) reg_value (-8);
    (* actual data *)
    assign_to_address ctx buf lhs reg_value (-16);
    assign_to_address ctx buf rhs reg_value (-24);
    let s = StackValue (turn_into_stack ctx buf reg_value) in
    free_register reg ctx;
    s
  | Binop.StringIndex ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    let ret = call_runtime_mlml ctx buf "get_string" [lhs; rhs] in
    StackValue (turn_into_stack ctx buf (RegisterValue ret))
  | Binop.ArrayIndex ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    let ret = call_runtime_mlml ctx buf "get_array" [lhs; rhs] in
    StackValue (turn_into_stack ctx buf (RegisterValue ret))
  | Binop.Custom _ -> failwith "custom infix operator is left in codegen"

and codegen_unaryop ctx buf e = function
  | Uop.Positate -> codegen_expr ctx buf e
  | Uop.Negate ->
    let e = codegen_expr ctx buf e in
    let res = constant_value 2 |> assign_to_new_register ctx buf in
    B.emit_inst_fmt buf "subq %s, %s" (string_of_value e) (string_of_register res);
    free_register res ctx;
    turn_into_stack ctx buf (RegisterValue res) |> stack_value

and codegen_expr ctx buf = function
  | Expr.Int num -> make_marked_const num
  | Expr.String s -> make_string_const ctx buf s
  | Expr.Format _ -> failwith "format string is left in codegen"
  | Expr.BinOp (op, lhs, rhs) -> codegen_binop ctx buf lhs rhs op
  | Expr.UnaryOp (op, e) -> codegen_unaryop ctx buf e op
  | Expr.App (lhs, rhs) ->
    let lhs = codegen_expr ctx buf lhs in
    let rhs = codegen_expr ctx buf rhs in
    let ret = safe_call ctx buf (Printf.sprintf "*%s" (string_of_value lhs)) [rhs] in
    StackValue (turn_into_stack ctx buf (RegisterValue ret))
  | Expr.Var ident -> StackValue (get_variable ctx ident)
  | Expr.LetAnd (is_rec, l, rhs) ->
    let pats, values = emit_let_binding_values ctx buf is_rec l in
    let def (name, ptr) = define_variable ctx buf name ptr in
    let undef (name, _) = undef_variable ctx name in
    List.iter def values;
    let rhs = codegen_expr ctx buf rhs in
    List.iter undef values;
    List.iter (undef_variable_pattern ctx) pats;
    rhs
  | Expr.Lambda (param, body) -> emit_function_value ctx buf false "_lambda" param body
  | Expr.IfThenElse (cond, then_, else_) ->
    let cond = codegen_expr ctx buf cond in
    let eval_stack = push_to_stack ctx buf (ConstantValue 0) in
    let else_label = new_unnamed_label ctx in
    let join_label = new_unnamed_label ctx in
    branch_if_falsy ctx buf cond else_label;
    (* then block *)
    let then_ = codegen_expr ctx buf then_ in
    assign_to_stack ctx buf then_ eval_stack;
    B.emit_inst_fmt buf "jmp %s" (string_of_label join_label);
    (* else block *)
    start_label buf else_label;
    let else_ = codegen_expr ctx buf else_ in
    assign_to_stack ctx buf else_ eval_stack;
    start_label buf join_label;
    StackValue eval_stack
  (* array is internally identical to tuple *)
  | Expr.Array values | Expr.Tuple values ->
    let values = List.map (codegen_expr ctx buf) values in
    make_tuple_const ctx buf values
  | Expr.Ctor (name, value) ->
    let value =
      match value with
      | Some value -> codegen_expr ctx buf value
      (* TODO: Better representation of ctor without parameters *)
      | None -> make_marked_const 0
    in
    let idx = get_ctor_index ctx name in
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    (* three 64-bit values -> 24 *)
    alloc_heap_ptr_constsize ctx buf 24 reg_value;
    (* size of data (2 * 8) *)
    assign_to_address ctx buf (ConstantValue (16 * 2)) reg_value 0;
    (* ctor index *)
    assign_to_address ctx buf (make_marked_const idx) reg_value (-8);
    (* the value *)
    assign_to_address ctx buf value reg_value (-16);
    let s = StackValue (turn_into_stack ctx buf reg_value) in
    free_register reg ctx;
    s
  | Expr.Match (v, arms) ->
    let v = codegen_expr ctx buf v in
    let join_label = new_unnamed_label ctx in
    let eval_stack = push_to_stack ctx buf (ConstantValue 0) in
    let rec aux = function
      | (pat, when_, rhs) :: t ->
        let next_label = new_unnamed_label ctx in
        pattern_match ctx buf pat v next_label;
        (match when_ with
        | Some cond ->
          let cond = codegen_expr ctx buf cond in
          branch_if_falsy ctx buf cond next_label
        | None -> ());
        let rhs = codegen_expr ctx buf rhs in
        assign_to_stack ctx buf rhs eval_stack;
        B.emit_inst_fmt buf "jmp %s" (string_of_label join_label);
        start_label buf next_label;
        aux t
      | [] ->
        B.emit_inst_fmt buf "jmp %s" (string_of_label match_fail_label);
        start_label buf join_label;
        StackValue eval_stack
    in
    aux arms
  | Expr.Nil ->
    let reg = alloc_register ctx in
    let reg_value = RegisterValue reg in
    (* size, flag -> 8 * 2 *)
    alloc_heap_ptr_constsize ctx buf 16 reg_value;
    (* data size *)
    assign_to_address ctx buf (ConstantValue (8 * 2)) reg_value 0;
    (* nil -> 0, cons -> 1 *)
    assign_to_address ctx buf (make_marked_const 0) reg_value (-8);
    let s = StackValue (turn_into_stack ctx buf reg_value) in
    free_register reg ctx;
    s
  | Expr.Record fields ->
    let trans (name, expr) = get_field_index ctx name, codegen_expr ctx buf expr in
    let cmp (i1, _) (i2, _) = compare i1 i2 in
    List.map trans fields |> List.sort cmp |> List.map snd |> make_tuple_const ctx buf
  | Expr.RecordField (v, field) ->
    let v = codegen_expr ctx buf v in
    let idx = get_field_index ctx field in
    let reg = alloc_register ctx in
    read_from_address ctx buf v (RegisterValue reg) (-(idx + 1) * 8);
    let s = StackValue (turn_into_stack ctx buf (RegisterValue reg)) in
    free_register reg ctx;
    s
  | Expr.RecordFieldAssign (v, field, e) ->
    let v = codegen_expr ctx buf v in
    let e = codegen_expr ctx buf e in
    let idx = get_field_index ctx field in
    assign_to_address ctx buf e v (-(idx + 1) * 8);
    (* Evaluates to unit *)
    make_tuple_const ctx buf []
  | Expr.RecordUpdate (target, fields) ->
    let target = codegen_expr ctx buf target in
    let reg = alloc_register ctx in
    shallow_copy ctx buf target (RegisterValue reg);
    let aux (name, v) =
      let v = codegen_expr ctx buf v in
      let i = get_field_index ctx name in
      assign_to_address ctx buf v (RegisterValue reg) (-(i + 1) * 8)
    in
    List.iter aux fields;
    let s = StackValue (turn_into_stack ctx buf (RegisterValue reg)) in
    free_register reg ctx;
    s
  | Expr.ArrayAssign (ary, idx, v) ->
    let ary = codegen_expr ctx buf ary in
    let idx = codegen_expr ctx buf idx in
    let v = codegen_expr ctx buf v in
    let ret = call_runtime_mlml ctx buf "set_array" [ary; idx; v] in
    StackValue (turn_into_stack ctx buf (RegisterValue ret))

and codegen_definition ctx buf = function
  | Mod.LetAnd (is_rec, l) ->
    let _, values = emit_let_binding_values ctx buf is_rec l in
    let def (name, ptr) = define_variable ctx buf name ptr in
    List.iter def values
  | Mod.TypeDef l ->
    let aux (_, _, def) = codegen_type_def ctx buf def in
    List.iter aux l
  | Mod.External (name, _ty, decl) ->
    let ptr = alloc_register ctx in
    label_ptr_to_register buf (Label decl) ptr;
    define_variable ctx buf name (RegisterValue ptr);
    free_register ptr ctx
  | Mod.Module _ -> failwith "Module is left!"
  | Mod.Open _ -> failwith "Open is left!"

and codegen_type_def ctx _buf = function
  | Mod.Variant variants ->
    let aux i (ctor, _) = define_ctor ctx ctor i in
    List.iteri aux variants
  | Mod.Record fields ->
    let aux i (_is_mut, name, _) = define_field ctx name i in
    List.iteri aux fields
  | Mod.Alias _ -> ()

and codegen_module_item ctx buf = function
  | Mod.Definition def -> codegen_definition ctx buf def
  | Mod.Expression expr ->
    let _ = codegen_expr ctx buf expr in
    ()

and codegen_module ctx buf = List.iter (codegen_module_item ctx buf)

and emit_function_with ctx main_buf label fn =
  let old_env = use_env ctx @@ new_local_env () in
  let buf = B.create () in
  let ret_label = new_unnamed_label ctx in
  start_global_label buf label;
  B.emit_inst_fmt buf "pushq %s" (register_name "rbp");
  B.emit_inst_fmt buf "movq %s, %s" (register_name "rsp") (register_name "rbp");
  let subq_place = B.emit_placeholder buf in
  (* save registers (non-volatile registers) *)
  let exclude_rbp_rsp = function
    | Register "rbp" | Register "rsp" -> false
    | _ -> true
  in
  let saver r = r, turn_into_stack ctx buf (RegisterValue r) in
  let saved_stacks =
    non_volatile_registers |> SS.filter exclude_rbp_rsp |> SS.elements |> List.map saver
  in
  fn ctx buf label ret_label;
  start_label buf ret_label;
  let stack_used = ctx.current_env.current_stack in
  let restore (r, s) = assign_to_register buf (StackValue s) r in
  List.iter restore saved_stacks;
  B.emit_inst_fmt buf "movq %s, %s" (register_name "rbp") (register_name "rsp");
  B.emit_inst_fmt buf "popq %s" (register_name "rbp");
  B.emit_inst buf "ret";
  let _ = use_env ctx old_env in
  B.substitute
    buf
    subq_place
    (B.Inst (Printf.sprintf "subq $%d, %%rsp" (-stack_used + 7)));
  B.prepend_buffer main_buf buf

and emit_let_bindings ctx buf is_rec l =
  (* TODO: remove `failwith "unreachable"` *)
  let funs, vars = List.partition Expr.is_fun_bind l in
  let make_convenient_data = function
    | Expr.FunBind (name, param, body) ->
      let label = new_label ctx name in
      (name, label), (label, param, body)
    | _ -> failwith "unreachable"
  in
  let labels, funs = List.map make_convenient_data funs |> List.split in
  let emit param ast ctx buf _label _ =
    let arg = nth_arg_stack ctx buf 0 in
    pattern_match ctx buf param (StackValue arg) match_fail_label;
    (if is_rec
    then
      (* forward definition of functions *)
      let aux (name, label) =
        let ptr = function_ptr ctx buf label in
        define_variable ctx buf name ptr
      in
      List.iter aux labels);
    let value = codegen_expr ctx buf ast in
    assign_to_register buf value ret_register
  in
  let aux_vars = function
    | Expr.VarBind (pat, body) ->
      let body = codegen_expr ctx buf body in
      pattern_match ctx buf pat body match_fail_label;
      pat
    | _ -> failwith "unreachable"
  in
  let aux_funs (label, param, body) =
    emit_function_with ctx buf label (emit param body)
  in
  (* emit variables first. *)
  (* functions can be forward reference, whereas variables can't. *)
  let pats = List.map aux_vars vars in
  List.iter aux_funs funs;
  pats, labels

and emit_function ctx main_buf is_rec name param ast =
  emit_let_bindings ctx main_buf is_rec [Expr.FunBind (name, param, ast)]
  |> snd
  |> List.hd
  |> snd

and emit_let_binding_values ctx buf is_rec l =
  let pats, labels = emit_let_bindings ctx buf is_rec l in
  let conv (name, label) = name, function_ptr ctx buf label in
  pats, List.map conv labels

and emit_function_value ctx buf is_rec name param ast =
  let label = emit_function ctx buf is_rec name param ast in
  function_ptr ctx buf label

and emit_main ctx buf label items =
  let emit ctx buf _label _ =
    let _ =
      safe_call ctx buf "GC_init@PLT" []
    in
    let argc, free1 = nth_arg_register ctx 0 in
    let argv, free2 = nth_arg_register ctx 1 in
    let _ =
      call_runtime ctx buf "handle_argv" [RegisterValue argc; RegisterValue argv]
    in
    free1 ctx;
    free2 ctx;
    codegen_module ctx buf items;
    assign_to_register buf (ConstantValue 0) ret_register
  in
  emit_function_with ctx buf label emit
;;

let emit_runtime ctx buf name f =
  let label = new_label ctx @@ make_name_of_runtime name in
  emit_function_with ctx buf label f
;;

let f ast =
  let buf = B.create () in
  let ctx = new_context () in
  Runtime.emit_all (emit_runtime ctx buf);
  emit_main ctx buf (Label "main") ast;
  B.prepend_sub_inst buf ".section .rodata";
  B.prepend_inst buf ".text";
  B.contents buf
;;
