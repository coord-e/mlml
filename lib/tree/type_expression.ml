type t =
  | Ident of string
  | Tuple of t list
  | Var of string
  | Ctor of t list * string

let rec string_of_type_expression = function
  | Ident ident -> Printf.sprintf "Ident %s" ident
  | Var ident -> Printf.sprintf "Var %s" ident
  | Ctor (tys, ident) ->
    let tys = List.map string_of_type_expression tys |> String.concat ", " in
    Printf.sprintf "Ctor (%s) %s" tys ident
  | Tuple ts ->
    let ts = List.map string_of_type_expression ts |> String.concat " * " in
    Printf.sprintf "Tuple (%s)" ts
;;
