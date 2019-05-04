(* simple (and inefficient) implementation of set *)

type 'a t = {data : 'a list}

let empty = {data = []}
let singleton x = {data = [x]}

let rec remove_duplicates = function
  | [] -> []
  | h :: t ->
    let t = List.filter (fun x -> x <> h) t in
    h :: remove_duplicates t
;;

let elements s = remove_duplicates s.data
let of_list l = {data = l}

let union a b =
  let l = elements a @ elements b in
  of_list l
;;

let mem e s = List.mem e s.data
let add e s = of_list (e :: s.data)

let remove e s =
  let l = List.filter (fun x -> x <> e) s.data in
  of_list l
;;

let choose_opt s = match s.data with [] -> None | h :: _ -> Some h
let choose s = match choose_opt s with Some v -> v | None -> failwith "Empty"

let diff a b =
  let l = List.filter (fun x -> mem x b) a.data in
  of_list l
;;

let filter f s = of_list (List.filter f (elements s))
