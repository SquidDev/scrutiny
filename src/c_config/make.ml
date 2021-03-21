let () =
  match Sys.argv with
  | [| _; ctypes |] -> Printf.printf "-I%s\n" (Filename.dirname ctypes)
  | _ -> ()
