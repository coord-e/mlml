let () =
  (* TODO: Remove hardcoded path *)
  let current_dir = "../../../test/" in
  Tester.file (Filename.concat current_dir "./bundler_simple/entry.ml") "50421231"
;;
