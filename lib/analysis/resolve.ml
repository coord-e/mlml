(* resolve paths and convert them into string *)
(* TODO: use more clear ways to resolve paths *)

module Path = Tree.Path
module Expr = Tree.Expression
module Mod = Tree.Module
module TyExpr = Tree.Type_expression
module Pat = Tree.Pattern
module NS = Tree.Namespace
module SS = Tree.Simple_set

type 'a value =
  | Entity of 'a
  (* absolute path *)
  | Alias of Path.t

and module_env =
  { vars : (string, unit value) Hashtbl.t
  ; types : (string, unit value) Hashtbl.t
  ; ctors : (string, unit value) Hashtbl.t
  ; fields : (string, unit value) Hashtbl.t
  ; modules : (string, module_env value) Hashtbl.t }

let create_module_env () =
  { vars = Hashtbl.create 32
  ; types = Hashtbl.create 32
  ; ctors = Hashtbl.create 32
  ; fields = Hashtbl.create 32
  ; modules = Hashtbl.create 32 }
;;

let mem_name_local env name =
  let or_else optb = function None -> optb | v -> v in
  Hashtbl.find_opt env.ctors name
  |> or_else (Hashtbl.find_opt env.vars name)
  |> or_else (Hashtbl.find_opt env.fields name)
  |> or_else (Hashtbl.find_opt env.types name)
;;

let find_module_local env name = Hashtbl.find_opt env.modules name

(* find a provided path in env and returns canonical path and `module_env` if found *)
let rec find_aux root_env path =
  (* same as `find_module_local`, but resolves the aliases *)
  (* returns `module_env` and canonical path if name is alias *)
  let find_module_env_local env name =
    match find_module_local env name with
    | Some (Entity v) -> Some (v, None)
    | Some (Alias path) ->
      (match find_aux root_env path with
      | Some (p, Some m) -> Some (m, Some p)
      | _ -> None)
    | None -> None
  in
  let rec aux env path resolved =
    match Path.extract path with
    | [head] ->
      let current_resolved = Path.join resolved (Path.single head) in
      (match find_module_env_local env head with
      | Some (e, Some p) -> Some (p, Some e)
      | Some (e, None) -> Some (current_resolved, Some e)
      | None ->
        (* not a module *)
        (match mem_name_local env head with
        | Some (Entity ()) -> Some (current_resolved, None)
        | Some (Alias path) -> find_aux root_env path
        | None -> None))
    | head :: tail ->
      (match find_module_env_local env head with
      | Some (e, None) ->
        aux e (Path.of_list tail) (Path.join resolved (Path.single head))
      | Some (e, Some p) -> aux e (Path.of_list tail) p
      | _ -> None)
    | [] -> None
  in
  aux root_env path Path.root

and find_module_opt env path =
  match find_aux env path with Some (_, Some m) -> Some m | _ -> None
;;

let find_module env path =
  match find_module_opt env path with Some v -> v | None -> failwith "NotFound"
;;

let canonical_opt env path =
  match find_aux env path with Some (p, _) -> Some p | None -> None
;;

(* `canonical env path` returns canonical form of `path` in `env` *)
let canonical env path =
  match canonical_opt env path with
  | Some p -> p
  | None ->
    failwith
    @@ Printf.sprintf "could not canonicalize path %s" (Path.string_of_path path)
;;

(* `mem env path` checks if `path` is reachable in `env` *)
let mem env path = match find_aux env path with Some _ -> true | None -> false

(* conversion context *)
(* TODO: Replace list with some other generic mutable set type *)
type context = {primary : Path.t}

let create_context () = {primary = Path.root}
let absolute ctx path = Path.join ctx.primary path
let absolute_name ctx name = absolute ctx (Path.single name)

let resolve env ctx path =
  let candidates = Path.subpaths ctx.primary in
  let make_abs c = Path.join c path in
  match List.find_opt (mem env) (List.map make_abs candidates) with
  | Some p -> canonical env p
  | None ->
    failwith @@ Printf.sprintf "could not resolve path %s" (Path.string_of_path path)
;;

(* convert a path to a pair of `module_env` and local name in returned env *)
let to_env_and_name env path =
  match Path.init_last path with
  | [], name -> env, name
  | init, last ->
    let m = find_module env (Path.of_list init) in
    m, last
;;

let add_local_with_ns env name v ns =
  let f =
    match ns with
    | NS.Var -> Hashtbl.add env.vars
    | NS.Ctor -> Hashtbl.add env.ctors
    | NS.Field -> Hashtbl.add env.fields
    | NS.Type -> Hashtbl.add env.types
  in
  f name v
;;

let add_local_name_with_ns env name ns = add_local_with_ns env name (Entity ()) ns
let add_local_alias_with_ns env name path ns = add_local_with_ns env name (Alias path) ns

let add_with_ns env path ns =
  let m, name = to_env_and_name env path in
  add_local_name_with_ns m name ns
;;

let mem_local_with_ns env name = function
  | NS.Var -> Hashtbl.mem env.vars name
  | NS.Ctor -> Hashtbl.mem env.ctors name
  | NS.Field -> Hashtbl.mem env.fields name
  | NS.Type -> Hashtbl.mem env.types name
;;

let mem_with_ns env path ns =
  let m, name = to_env_and_name env path in
  mem_local_with_ns m name ns
;;

let insert_alias env path target =
  let m, name = to_env_and_name env path in
  Hashtbl.add m.modules name (Alias target)
;;

let iter_names f env =
  let apply ns k _ = f k ns in
  Hashtbl.iter (apply NS.Var) env.vars;
  Hashtbl.iter (apply NS.Ctor) env.ctors;
  Hashtbl.iter (apply NS.Field) env.fields;
  Hashtbl.iter (apply NS.Type) env.types
;;

let alias_names from_path from to_ =
  let adder v ns =
    let abs = Path.join from_path (Path.single v) in
    add_local_alias_with_ns to_ v abs ns
  in
  iter_names adder from;
  let adder_module k _ =
    let abs = Path.join from_path (Path.single k) in
    Hashtbl.add to_.modules k (Alias abs)
  in
  Hashtbl.iter adder_module from.modules
;;

let alias_names env ctx path =
  let from = find_module env path in
  let to_ = find_module env ctx.primary in
  copy_names path from to_
;;

let in_new_module env ctx name f =
  let path = absolute_name ctx name in
  let m, name = to_env_and_name env path in
  Hashtbl.add m.modules name (Entity (create_module_env ()));
  f {primary = path}
;;

(* expression-local environment *)
(* TODO: Replace this with some other generic mutable set type *)
type local_env = {mutable local_vars : string SS.t}

let create_local_env () = {local_vars = SS.empty}

(* the main conversion *)
let apply_binds local_env x = function
  | NS.Var ->
    local_env.local_vars <- SS.add x local_env.local_vars;
    x
  | _ -> failwith "unexpected binding"
;;

let apply_vars local_env env ctx path ns =
  let path =
    match ns, Path.extract path with
    (* locally-bound variables *)
    | NS.Var, [head] when SS.mem head local_env.local_vars -> path
    | _ -> resolve env ctx path
  in
  Path.string_of_path path
;;

let convert_expr' local_env env ctx expr =
  Expr.apply_on_names (apply_vars local_env env ctx) (apply_binds local_env) expr
;;

let convert_expr env ctx expr = convert_expr' (create_local_env ()) env ctx expr

let convert_type_expr env ctx expr =
  let binds x _ = x in
  let vars x _ =
    let path = resolve env ctx x in
    Path.string_of_path path
  in
  TyExpr.apply_on_names vars binds expr
;;

let convert_type_def env ctx defn =
  match defn with
  | Mod.Variant l ->
    let aux (ctor_name, expr_opt) =
      let ctor_name = absolute_name ctx ctor_name in
      add_with_ns env ctor_name NS.Ctor;
      let expr_opt =
        match expr_opt with Some e -> Some (convert_type_expr env ctx e) | None -> None
      in
      Path.string_of_path ctor_name, expr_opt
    in
    Mod.Variant (List.map aux l)
  | Mod.Record l ->
    let aux (is_mut, field_name, expr) =
      let field_name = absolute_name ctx field_name in
      add_with_ns env field_name NS.Field;
      let expr = convert_type_expr env ctx expr in
      is_mut, Path.string_of_path field_name, expr
    in
    Mod.Record (List.map aux l)
  | Mod.Alias expr ->
    let expr = convert_type_expr env ctx expr in
    Mod.Alias expr
;;

let rec convert_defn env ctx defn =
  match defn with
  | Mod.LetAnd (is_rec, l) ->
    let local_env = create_local_env () in
    let binds is_local x ns =
      match is_local with
      | true -> apply_binds local_env x ns
      | false ->
        let path = absolute_name ctx x in
        add_with_ns env path ns;
        Path.string_of_path path
    in
    let l = Expr.apply_on_let_bindings (apply_vars local_env env ctx) binds is_rec l in
    [Mod.Definition (Mod.LetAnd (is_rec, l))]
  | Mod.TypeDef l ->
    let intros (tyvars, bind, def) =
      let bind = absolute_name ctx bind in
      add_with_ns env bind NS.Type;
      tyvars, bind, def
    in
    let aux (tyvars, bind, def) =
      let def = convert_type_def env ctx def in
      tyvars, Path.string_of_path bind, def
    in
    let l = List.map intros l |> List.map aux in
    [Mod.Definition (Mod.TypeDef l)]
  | Mod.Module (name, Mod.Path path) ->
    let t = absolute_name ctx name in
    let path = resolve env ctx path in
    insert_alias env t path;
    []
  | Mod.Module (name, Mod.Struct l) ->
    let f ctx = List.map (convert_module_item env ctx) l |> List.flatten in
    in_new_module env ctx name f
  | Mod.Open path ->
    let path = resolve env ctx path in
    open_path env ctx path;
    []
  | Mod.External (name, ty, decl) ->
    let path = absolute_name ctx name in
    add_with_ns env path NS.Var;
    let name = Path.string_of_path path in
    let ty = convert_type_expr env ctx ty in
    [Mod.Definition (Mod.External (name, ty, decl))]

and convert_module_item env ctx = function
  | Mod.Expression expr -> [Mod.Expression (convert_expr env ctx expr)]
  | Mod.Definition defn -> convert_defn env ctx defn
;;

let add_primitives env =
  let types = ["unit"; "int"; "bool"; "char"; "string"; "bytes"; "array"; "list"] in
  let adder x = add_local_name_with_ns env x NS.Type in
  List.iter adder types
;;

let f l =
  let env = create_module_env () in
  let ctx = create_context () in
  add_primitives env;
  List.map (convert_module_item env ctx) l |> List.flatten
;;
