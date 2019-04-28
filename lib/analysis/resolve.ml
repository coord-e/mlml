(* resolve paths and convert them into string *)

module Path = Tree.Path
module Expr = Tree.Expression
module Mod = Tree.Module
module Pat = Tree.Pattern
module NS = Tree.Namespace
module SS = Set.Make (String)

type module_value =
  | Env of module_env
  (* absolute path *)
  | Alias of Path.t

and module_env =
  { mutable vars : SS.t
  ; mutable types : SS.t
  ; mutable ctors : SS.t
  ; mutable fields : SS.t
  ; modules : (string, module_value) Hashtbl.t }

let create_module_env () =
  { vars = SS.empty
  ; types = SS.empty
  ; ctors = SS.empty
  ; fields = SS.empty
  ; modules = Hashtbl.create 32 }
;;

(* `path_of_value path v` returns `path` if `v` is not alias, and returns the aliased
   destination if `v` is alias *)
let path_of_value path = function Env _ -> path | Alias p -> p

let mem_name_local env name =
  Hashtbl.mem env.modules name
  || SS.mem name env.ctors
  || SS.mem name env.vars
  || SS.mem name env.fields
;;

let find_module_local env name = Hashtbl.find_opt env.modules name

(* `env_of_value env v` returns a `module_env` corresponding to `v`, with resolving alias *)
let rec env_of_value_opt env = function
  | Env v -> Some v
  | Alias path -> find_module_opt env path

(* find a provided path in env and returns canonical path and `module_env` if found *)
and find_aux root_env path =
  (* same as `find_module_local`, but resolves the aliases *)
  let find_local_aux env path =
    match find_module_local env path with
    (* use root env here to resolve aliases *)
    | Some v -> Some v, env_of_value_opt root_env v
    | None -> None, None
  in
  let rec aux env path resolved =
    match Path.extract path with
    | [head] ->
      (match mem_name_local env head with
      | true ->
        Some (resolved, find_local_aux env head |> snd)
      | false -> None)
    | head :: tail ->
      (match find_local_aux env head with
      | Some v, Some e -> aux e (Path.of_list tail) (path_of_value path v)
      | _ -> None)
    | [] -> None
  in
  aux root_env path (Path.last_path path)

and find_module_opt env path =
  match find_aux env path with Some (_, Some m) -> Some m | _ -> None
;;

let find_module env path =
  match find_module_opt env path with Some v -> v | None -> failwith "NotFound"
;;

(* `canonical env path` returns canonical form of `path` in `env` *)
let canonical env path =
  (* Keep the original form when not found to support external functions *)
  (* TODO: Remove this behavior after an implementation of "external"    *)
  match find_aux env path with Some (p, _) -> p | None -> path
;;

(* `mem env path` checks if `path` is reachable in `env` *)
let mem env path = match find_aux env path with Some _ -> true | None -> false

let resolve env current path =
  (* resolve aliases first *)
  let path = canonical env path in
  match mem env path with
  (* the path is relative *)
  | true -> Path.join current path
  (* the path is absolute *)
  | false -> path
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
;;

let add_with_ns env path ns =
  let m, name = to_env_and_name env path in
  add_local_with_ns m name ns
;;

let mem_local_with_ns env name = function
  | NS.Var -> SS.mem name env.vars
  | NS.Ctor -> SS.mem name env.ctors
  | NS.Field -> SS.mem name env.fields
;;

let mem_with_ns env path ns =
  let m, name = to_env_and_name env path in
  mem_local_with_ns m name ns
;;

let insert_alias env path target =
  let m, name = to_env_and_name env path in
  Hashtbl.add m.modules name (Alias target)
;;

let in_new_module env current name f =
  let path = Path.join current (Path.single name) in
  let new_env = create_module_env () in
  let res = f new_env path in
  Hashtbl.add env.modules name (Env new_env);
  res
;;

(* expression-local environment *)
(* TODO: Replace this with some other generic mutable set type *)
type local_env = {mutable local_vars : SS.t}

let create_local_env () = {local_vars = SS.empty}

(* the main conversion *)
let apply_binds local_env x = function
  | NS.Var ->
    local_env.local_vars <- SS.add x local_env.local_vars;
    x
  | _ -> failwith "unexpected binding"
;;

let apply_vars local_env env current path ns =
  let path =
    match ns, Path.extract path with
    (* locally-bound variables *)
    | NS.Var, [head] when SS.mem head local_env.local_vars -> path
    | _ -> resolve env current path
  in
  Path.string_of_path path
;;

let convert_expr' local_env env current expr =
  Expr.apply_on_names (apply_vars local_env env current) (apply_binds local_env) expr
;;

let convert_expr env current expr = convert_expr' (create_local_env ()) env current expr

let rec convert_defn env current defn =
  match defn with
  | Mod.LetAnd (is_rec, l) ->
    let aux = function
      | Expr.VarBind (p, body) ->
        let binds x ns =
          let path = Path.join current (Path.single x) in
          add_with_ns env path ns;
          Path.string_of_path path
        and vars x _ns =
          let path = resolve env current x in
          Path.string_of_path path
        in
        let body = convert_expr env current body in
        let p = Pat.apply_on_names vars binds p in
        Expr.VarBind (p, body)
      | Expr.FunBind (bind, p, body) ->
        let inner_env = create_local_env () in
        let f = apply_vars inner_env env current in
        let g = apply_binds inner_env in
        let p = Pat.apply_on_names f g p in
        let body = Expr.apply_on_names f g body in
        let bind = Path.join current (Path.single bind) in
        add_with_ns env bind NS.Var;
        Expr.FunBind (Path.string_of_path bind, p, body)
    in
    [Mod.Definition (Mod.LetAnd (is_rec, List.map aux l))]
  | Mod.TypeDef _ -> failwith "unimplemented"
  | Mod.Module (name, Mod.Path path) ->
    let t = Path.join current (Path.single name) in
    insert_alias env t path;
    []
  | Mod.Module (name, Mod.Struct l) ->
    let f env current = List.map (convert_module_item env current) l |> List.flatten in
    in_new_module env current name f

and convert_module_item env current = function
  | Mod.Expression expr -> [Mod.Expression (convert_expr env current expr)]
  | Mod.Definition defn -> convert_defn env current defn
;;

let f l =
  List.map (convert_module_item (create_module_env ()) Path.root) l |> List.flatten
;;
