open struct
  module Log = (val Logs.src_log (Logs.Src.create __FILE__))
end

let run_command cmd =
  Log.info (fun f -> f "Running %a" Fmt.(array ~sep:Fmt.sp string) cmd);
  let proc = Unix.create_process cmd.(0) cmd Unix.stdin Unix.stdout Unix.stderr in
  match%lwt Lwt_unix.wait4 [] proc with
  | _, WEXITED 0, _ -> Lwt.return_unit
  | _ -> failwith "Process exited"

let with_temp ?(prefix = "scrutiny") ?(suffix = "") fn =
  let file = Filename.temp_file prefix suffix in
  Lwt.finalize
    (fun () -> fn file)
    (fun () -> try%lwt Lwt_unix.unlink file with Unix.Unix_error _ -> Lwt.return_unit)

let with_timeout ~timeout fn = Lwt_unix.with_timeout timeout fn
let current_env : Eio.Stdenv.t Eio.Fiber.key = Eio.Fiber.create_key ()
