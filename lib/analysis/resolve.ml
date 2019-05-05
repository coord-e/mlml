(* resolve paths and convert them into string *)
(* TODO: use more clear ways to resolve paths *)

module Path = Tree.Path
module Expr = Tree.Expression
module Mod = Tree.Module
module TyExpr = Tree.Type_expression
module Pat = Tree.Pattern
module NS = Tree.Namespace
module SS = Tree.Simple_set

type module_value =
  | Env of module_env
  (* absolute path *)
  | Alias of Path.t

and module_env =
  { mutable vars : string SS.t
  ; mutable types : string SS.t
  ; mutable ctors : string SS.t
  ; mutable fields : string SS.t
  ; modules : (string, module_value) Hashtbl.t }

let create_module_env () =
  { vars = SS.empty
  ; types = SS.empty
  ; ctors = SS.empty
  ; fields = SS.empty
  ; modules = Hashtbl.create 32 }
;;

let mem_name_local env name =
  Hashtbl.mem env.modules name
  || SS.mem name env.ctors
  || SS.mem name env.vars
  || SS.mem name env.fields
;;

let find_module_local env name = Hashtbl.find_opt env.modules name

(* find a provided path in env and returns canonical path and `module_env` if found *)
let rec find_aux root_env path =
  (* same as `find_module_local`, but resolves the aliases *)
  (* returns `module_env` and canonical path if name is alias *)
  let find_module_env_local env name =
    match find_module_local env name with
    | Some (Env v) -> Some (v, None)
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
        | true -> Some (current_resolved, None)
        | false -> None))
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
type context =
  { primary : Path.t
  ; mutable opened_paths : Path.t list }

let create_context () = {primary = Path.root; opened_paths = []}
let open_path ctx path = ctx.opened_paths <- path :: ctx.opened_paths
let absolute ctx path = Path.join ctx.primary path
let absolute_name ctx name = absolute ctx (Path.single name)

let resolve env ctx path =
  (* resolve aliases first *)
  let path = canonical env path in
  let subpaths = Path.subpaths ctx.primary in
  let candidates = ctx.opened_paths @ subpaths in
  let make_abs c = Path.join c path in
  match List.find_opt (mem env) (List.map make_abs candidates) with
  (* the path is relative *)
  | Some p -> p
  (* the path is absolute *)
  | None -> path
;;

(* convert a path to a pair of `module_env` and local name in returned env *)
let to_env_and_name env path =
  match Path.init_last path with
  | [], name -> env, name
  | init, last ->
    let m = find_module env (Path.of_list init) in
    m, last
;;

let add_local_with_ns env name = function
  | NS.Var -> env.vars <- SS.add name env.vars
  | NS.Ctor -> env.ctors <- SS.add name env.ctors
  | NS.Field -> env.fields <- SS.add name env.fields
  | NS.Type -> env.types <- SS.add name env.types
;;

let add_with_ns env path ns =
  let m, name = to_env_and_name env path in
  add_local_with_ns m name ns
;;

let mem_local_with_ns env name = function
  | NS.Var -> SS.mem name env.vars
  | NS.Ctor -> SS.mem name env.ctors
  | NS.Field -> SS.mem name env.fields
  | NS.Type -> SS.mem name env.types
;;

let mem_with_ns env path ns =
  let m, name = to_env_and_name env path in
  mem_local_with_ns m name ns
;;

let insert_alias env path target =
  let m, name = to_env_and_name env path in
  Hashtbl.add m.modules name (Alias target)
;;

let in_new_module env ctx name f =
  let path = absolute_name ctx name in
  let m, name = to_env_and_name env path in
  Hashtbl.add m.modules name (Env (create_module_env ()));
  f {ctx with primary = path}
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
    let aux = function
      | Expr.VarBind (p, body) ->
        let binds x ns =
          let path = absolute_name ctx x in
          add_with_ns env path ns;
          Path.string_of_path path
        and vars x _ns =
          let path = resolve env ctx x in
          Path.string_of_path path
        in
        let body = convert_expr env ctx body in
        let p = Pat.apply_on_names vars binds p in
        Expr.VarBind (p, body)
      | Expr.FunBind (bind, p, body) ->
        let inner_env = create_local_env () in
        let f = apply_vars inner_env env ctx in
        let g = apply_binds inner_env in
        let bind = absolute_name ctx bind in
        (* TODO: Improve control flow *)
        if is_rec then add_with_ns env bind NS.Var;
        (* body conversion *)
        let p = Pat.apply_on_names f g p in
        let body = Expr.apply_on_names f g body in
        (* bound name (non-rec) *)
        if not is_rec then add_with_ns env bind NS.Var;
        Expr.FunBind (Path.string_of_path bind, p, body)
    in
    [Mod.Definition (Mod.LetAnd (is_rec, List.map aux l))]
  | Mod.TypeDef l ->
    let aux (tyvars, bind, def) =
      let bind = absolute_name ctx bind in
      let def = convert_type_def env ctx def in
      tyvars, Path.string_of_path bind, def
    in
    [Mod.Definition (Mod.TypeDef (List.map aux l))]
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
    open_path ctx path;
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

let f l =
  let env = create_module_env () in
  let ctx = create_context () in
  List.map (convert_module_item env ctx) l |> List.flatten
;;
