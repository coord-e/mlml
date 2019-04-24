(* resolve paths and convert them into string *)

module Path = Tree.Path
module Expr = Tree.Expression
module Mod = Tree.Module
module SS = Set.Make (String)

type module_env =
  { vars : SS.t
  ; types : SS.t
  ; ctors : SS.t
  ; fields : SS.t
  ; modules : (string, module_env) Hashtbl.t
  ; path : Path.t }

let create_module_env () =
  { vars = SS.empty
  ; types = SS.empty
  ; ctors = SS.empty
  ; fields = SS.empty
  ; modules = Hashtbl.create 32
  ; path = Path.root }
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

(* the main conversion *)
let convert_expr' local_env env expr =
  let binds x _ns =
    Hashtbl.add local_env x ();
    x
  and refs path _ns =
    let path =
      if Path.is_single path && (not @@ Hashtbl.mem local_env (Path.head path))
      then resolve env path
      else path
    in
    Path.string_of_path path
  in
  Expr.apply_on_names refs binds expr
;;

let convert_expr env expr = convert_expr' (Hashtbl.create 32) env expr

let convert_module_item env = function
  | Mod.Expression expr -> [Mod.Expression (convert_expr env expr)]
  | Mod.Definition _defn -> failwith "unimplemented"
;;

let f l = List.map (convert_module_item @@ create_module_env ()) l |> List.flatten
