open struct
  module Log = (val Logs.src_log (Logs.Src.create __FILE__))
end

let run_command cmd =
  Log.info (fun f -> f "Running %a" Fmt.(array ~sep:Fmt.sp string) cmd);
  Eio.Switch.run @@ fun sw ->
  let proc = Eio_process.spawn ~sw cmd.(0) cmd in
  match Eio_process.wait proc with
  | WEXITED 0 -> ()
  | _ -> failwith "Process exited"

let with_temp ?(prefix = "scrutiny") ?(suffix = "") ~fs fn =
  let file = Filename.temp_file prefix suffix in
  let file = Eio.Path.(fs / file) in
  Fun.protect
    ~finally:(fun () -> try Eio.Path.unlink file with Unix.Unix_error _ -> ())
    (fun () -> fn file)

let with_timeout ~clock ~timeout fn = Eio.Time.with_timeout_exn clock timeout fn
