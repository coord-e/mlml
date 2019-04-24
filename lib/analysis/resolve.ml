(* resolve paths and convert them into string *)

type SS = Set.Make(String)

type module_env =
  { vars : SS.t
  ; modules : (string, module_env) Hashtbl.t }

type ctx =
  { env : module_env
  ; current : Path.t }

let create_module_env () = { vars = SS.empty; modules = Hashtbl.create 32 }
let create_ctx () = { env = create_module_env (); current = Path.root }

(* context-independent operations *)
let rec mem env path =
  match Path.extract path with
  | [head] ->
    (match Path.is_capitalized head with
    | true -> Hashtbl.mem env.modules head
    | false -> List.mem env.vars head)
  | head :: tail ->
    (match Hashtbl.find_opt env.modules head with
    | Some v -> mem v (Path.of_list tail)
    | None -> false)
  | [] -> failwith "Empty"
;;

let rec find_module_opt env path =
  let head, tail = Path.head_tail path in
  match tail, Hashtbl.find_opt env.modules head with
  | [], Some v -> v
  | _, Some v -> mem v (Path.of_list tail)
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

(* context-dependent operations *)

(* obtain a path of newly-defining name *)
let new_name_path ctx name = Path.join ctx.current (Path.single name)

(* obtain a canonical path from raw path *)
let resolve ctx path =
  let abs = Path.join ctx.current path in
  match mem ctx.env abs with true -> abs | false -> path
;;

(* create an alias of `path` in current module *)
let alias ctx name path =
  let target_path = new_name_path ctx name in
  let alias_path = resolve ctx path in
  let alias_module = find_module ctx.env alias_path in
  insert_module ctx.env target_path alias_module
;;
