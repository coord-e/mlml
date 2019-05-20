external _mlml_get_argv : unit -> string array = "_mlml_get_argv"
external file_exists : string -> bool = "_mlml_file_exists"
external is_directory : string -> bool = "_mlml_is_directory"
external getcwd : unit -> string = "_mlml_getcwd"
external getenv : string -> string = "_mlml_getenv"
external _has_env : string -> bool = "_mlml_has_env"
external readdir : string -> string array = "_mlml_readdir"

let argv = _mlml_get_argv ()

let getenv_opt name =
  match _has_env name with true -> Some (getenv name) | false -> None
;;
