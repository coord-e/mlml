type 'a t =
  | Var of 'a
  | Int of int
  | String of string
  | Tuple of 'a t list
  | Ctor of 'a * 'a t option
  | Or of 'a t * 'a t
  | Cons of 'a t * 'a t
  | Nil
  | Record of ('a * 'a t) list
  | Range of char * char

let rec string_of_pattern f = function
  | Var x -> f x
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
