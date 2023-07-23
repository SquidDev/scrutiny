open struct
  module Log = (val Logs.src_log (Logs.Src.create __FILE__))
end

let run_command ~mgr cmd =
  Log.info (fun f -> f "Running %a" Fmt.(list ~sep:Fmt.sp string) cmd);
  Eio.Process.run mgr cmd

let with_temp ?(prefix = "scrutiny") ?(suffix = "") ~fs fn =
  let file = Filename.temp_file prefix suffix in
  let file = Eio.Path.(fs / file) in
  Fun.protect
    ~finally:(fun () -> try Eio.Path.unlink file with Eio.Io (Eio.Fs.E _, _) -> ())
    (fun () -> fn file)

let with_timeout ~clock ~timeout fn = Eio.Time.with_timeout_exn clock timeout fn
