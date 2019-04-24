type 'a t =
  | Ident of 'a
  | Tuple of 'a t list
  | Var of string
  | Ctor of 'a t list * 'a

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
