type 'a t =
  | Var of string
  | Wildcard
  | Int of int
  | String of string
  | Tuple of 'a t list
  | Ctor of 'a * 'a t option
  | Or of 'a t * 'a t
  | Cons of 'a t * 'a t
  | Nil
  | Record of ('a * 'a t) list
  | Range of char * char

(* apply `f` on reference names, apply `g` on binding names *)
let rec apply_on_names f g p =
  let apply = apply_on_names f g in
  match p with
  | Var bind -> Var (g bind)
  | Wildcard | Int _ | String _ | Nil | Range _ -> p
  | Tuple l -> Tuple (List.map apply l)
  | Ctor (name, None) -> Ctor (f name, None)
  | Ctor (name, Some v) -> Ctor (f name, Some (apply v))
  | Or (a, b) -> Or (apply a, apply b)
  | Cons (a, b) -> Cons (apply a, apply b)
  | Recprd l ->
    let aux (name, p) = f name, apply p in
    Record (List.map aux l)
;;

let rec string_of_pattern f = function
  | Var x -> x
  | Wildcard -> "_"
  | Int x -> string_of_int x
  | String s -> Printf.sprintf "\"%s\"" s
  | Tuple values ->
    List.map (string_of_pattern f) values |> String.concat ", " |> Printf.sprintf "(%s)"
  | Ctor (name, rhs) ->
    (match rhs with
    | Some rhs -> Printf.sprintf "%s (%s)" (f name) (string_of_pattern f rhs)
    | None -> name)
  | Or (a, b) ->
    Printf.sprintf "(%s) | (%s)" (string_of_pattern f a) (string_of_pattern f b)
  | Cons (a, b) ->
    Printf.sprintf "(%s) :: (%s)" (string_of_pattern f a) (string_of_pattern f b)
  | Nil -> "[]"
  | Record fields ->
    let aux (name, expr) =
      Printf.sprintf "%s = (%s)" (f name) (string_of_pattern f expr)
    in
    List.map aux fields |> String.concat "; " |> Printf.sprintf "{%s}"
  | Range (from, to_) -> Printf.sprintf "'%c' .. '%c'" from to_
;;

module SS = Set.Make (String)

let rec introduced_idents = function
  | Var x -> SS.singleton x
  | Wildcard -> SS.empty
  | Int _ | String _ -> SS.empty
  | Tuple values -> List.map introduced_idents values |> List.fold_left SS.union SS.empty
  | Ctor (_, value) ->
    (match value with Some value -> introduced_idents value | None -> SS.empty)
  | Or (a, b) -> SS.union (introduced_idents a) (introduced_idents b)
  | Cons (a, b) -> SS.union (introduced_idents a) (introduced_idents b)
  | Nil | Range _ -> SS.empty
  | Record fields ->
    let aux (_, p) = introduced_idents p in
    List.map aux fields |> List.fold_left SS.union SS.empty
;;

let introduced_ident_list p = introduced_idents p |> SS.elements
