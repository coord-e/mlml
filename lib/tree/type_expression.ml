module NS = Namespace

type 'a t =
  | Ident of 'a
  | Tuple of 'a t list
  | Var of string
  | Ctor of 'a t list * 'a

(* apply `f` on reference names, apply `g` on binding names *)
let rec apply_on_names f g e =
  let apply = apply_on_names f g in
  match e with
  | Ident x -> Ident (f x NS.Type)
  | Tuple l -> Tuple (List.map apply l)
  | Var s -> Var s
  | Ctor (params, ctor) ->
    let params = List.map apply params in
    let ctor = f ctor NS.Type in
    Ctor (params, ctor)
;;

let rec string_of_type_expression f = function
  | Ident ident -> Printf.sprintf "Ident %s" (f ident)
  | Var ident -> Printf.sprintf "Var %s" ident
  | Ctor (tys, ident) ->
    let tys = List.map (string_of_type_expression f) tys |> String.concat ", " in
    Printf.sprintf "Ctor (%s) %s" tys (f ident)
  | Tuple ts ->
    let ts = List.map (string_of_type_expression f) ts |> String.concat " * " in
    Printf.sprintf "Tuple (%s)" ts
;;
