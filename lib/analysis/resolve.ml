(* resolve paths and convert them into string *)

type module_env =
  { vars : (string, int) Hashtbl.t
  ; modules : (string, module_env) Hashtbl.t }

type ctx =
  { env : module_env
  ; current : Path.t }
