(* resolve paths and convert them into string *)

module Path = Tree.Path
module Expr = Tree.Expression
module Mod = Tree.Module
module Pat = Tree.Pattern
module NS = Tree.Namespace
module SS = Set.Make (String)

let module_value =
  | Env of module_env
  (* absolute path *)
  | Alias of Path.t

type module_env =
  { mutable vars : SS.t
  ; mutable types : SS.t
  ; mutable ctors : SS.t
  ; mutable fields : SS.t
  ; modules : (string, module_value) Hashtbl.t }

let create_module_env =
  { vars = SS.empty
  ; types = SS.empty
  ; ctors = SS.empty
  ; fields = SS.empty
  ; modules = Hashtbl.create 32 }
;;

let rec mem env path =
  match Path.extract path with
  | [head] ->
    (match Path.is_capitalized path with
    | true -> Hashtbl.mem env.modules head || SS.mem head env.ctors
    | false -> SS.mem head env.vars || SS.mem head env.fields)
  | head :: tail ->
    (match Hashtbl.find_opt env.modules head with
    | Some v -> mem v (Path.of_list tail)
    | None -> false)
  | [] -> failwith "Empty"
;;

let rec find_module_opt env path =
  let head, tail = Path.head_tail path in
  match tail, Hashtbl.find_opt env.modules head with
  | [], Some v -> Some v
  | _, Some v -> find_module_opt v (Path.of_list tail)
  | _, None -> None
;;

let find_module env path =
  match find_module_opt env path with Some v -> v | None -> failwith "NotFound"
;;

let rec insert_module env path m =
  match Path.extract path with
  | [head] -> Hashtbl.replace env.modules head m
  | head :: tail -> insert_module (Hashtbl.find env.modules head) (Path.of_list tail) m
  | [] -> failwith "Inserting module to a empty path"
;;

let canonical env path = Path.join env.path path

let resolve env path =
  match mem env path with
  (* the path is relative *)
  | true -> canonical env path
  (* the path is absolute *)
  | false -> path
;;

let add_name env name = function
  | NS.Var -> env.vars <- SS.add name env.vars
  | NS.Ctor -> env.ctors <- SS.add name env.ctors
  | NS.Field -> env.fields <- SS.add name env.fields
;;

let mem_name env name = function
  | NS.Var -> SS.mem name env.vars
  | NS.Ctor -> SS.mem name env.ctors
  | NS.Field -> SS.mem name env.fields
;;

let in_new_module env name f =
  let path = canonical env (Path.single name) in
  let new_env = create_module_env path in
  let res = f new_env in
  Hashtbl.add env.modules name new_env;
  res
;;

(* the main conversion *)
let apply_binds local_env x ns =
  add_name local_env x ns;
  x
;;

let apply_vars local_env env path ns =
  let path =
    if Path.is_single path && mem_name local_env (Path.head path) ns
    then path
    else resolve env path
  in
  Path.string_of_path path
;;

let convert_expr' local_env env expr =
  Expr.apply_on_names (apply_vars local_env env) (apply_binds local_env) expr
;;

let convert_expr env expr = convert_expr' (create_module_env Path.root) env expr

let rec convert_defn env defn =
  match defn with
  | Mod.LetAnd (is_rec, l) ->
    let aux = function
      | Expr.VarBind (p, body) ->
        let binds x ns =
          add_name env x ns;
          let path = canonical env (Path.single x) in
          Path.string_of_path path
        and vars x _ns =
          let path = resolve env x in
          Path.string_of_path path
        in
        let body = convert_expr env body in
        let p = Pat.apply_on_names vars binds p in
        Expr.VarBind (p, body)
      | Expr.FunBind (bind, p, body) ->
        let inner_env = create_module_env Path.root in
        let f = apply_vars inner_env env in
        let g = apply_binds inner_env in
        let p = Pat.apply_on_names f g p in
        let body = Expr.apply_on_names f g body in
        add_name env bind NS.Var;
        let bind = canonical env (Path.single bind) in
        Expr.FunBind (Path.string_of_path bind, p, body)
    in
    [Mod.Definition (Mod.LetAnd (is_rec, List.map aux l))]
  | Mod.TypeDef _ -> failwith "unimplemented"
  | Mod.Module (_name, Mod.Path _) -> failwith "unimplemented"
  | Mod.Module (name, Mod.Struct l) ->
    let f env = List.map (convert_module_item env) l |> List.flatten in
    in_new_module env name f

and convert_module_item env = function
  | Mod.Expression expr -> [Mod.Expression (convert_expr env expr)]
  | Mod.Definition defn -> convert_defn env defn
;;

let f l = List.map (convert_module_item @@ create_module_env Path.root) l |> List.flatten
