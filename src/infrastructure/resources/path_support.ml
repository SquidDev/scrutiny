let of_fpath ~env path = Eio.Path.(env#fs / Fpath.to_string path)

let mkdirs ~env ?(perm = 0o755) root =
  let rec go xs path =
    let path_s = Fpath.to_string path in
    let path_e = Eio.Path.(env#fs / path_s) in
    if Sys.file_exists path_s then List.iter (fun p -> Eio.Path.mkdir ~perm p) xs
    else if Fpath.is_root path then failwith "Root directory doesn't exist!"
    else go (path_e :: xs) Fpath.(parent path |> normalize)
  in
  go [] root
