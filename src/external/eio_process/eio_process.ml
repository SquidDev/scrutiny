open Eio.Std

(** Like {!Eio_unix.run_in_systhread} but slightly cheaty in that it can be cancelled.

    This doesn't actually cancel the operation, it just runs it as a promise so it doesn't matter. *)
let run_cancellable_in_systhread fn =
  let p, w = Promise.create () in
  Thread.create
    (fun () ->
      match fn () with
      | x -> Promise.resolve_ok w x
      | exception e -> Promise.resolve_error w e)
    ()
  |> ignore;
  Promise.await_exn p

type t = { pid : int }

let wait { pid } = run_cancellable_in_systhread (fun () -> Unix.waitpid [] pid) |> snd

let status { pid } =
  match Unix.waitpid [ WNOHANG ] pid with
  | 0, _ -> None
  | _, status -> Some status

let kill ~signal { pid } = Unix.kill pid signal

let spawn ~sw ?(stdin = Unix.stdin) ?(stdout = Unix.stdout) ?(stderr = Unix.stderr) prog argv =
  let pid =
    Eio_unix.run_in_systhread (fun () -> Unix.create_process prog argv stdin stdout stderr)
  in

  let cleanup () =
    match run_cancellable_in_systhread (fun () -> Unix.waitpid [] pid) with
    | _ -> ()
    | exception Unix.Unix_error (ECHILD, _, _) -> ()
  in
  Switch.on_release sw cleanup; { pid }

type channels = {
  stdin : Eio.Flow.sink;
  stdout : Eio.Flow.source;
  stderr : Eio.Flow.source;
}

let spawn_full ~sw prog argv =
  let stdin_r, stdin_w = Unix.pipe ~cloexec:true ()
  and stdout_r, stdout_w = Unix.pipe ~cloexec:true ()
  and stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let pid = spawn ~sw ~stdin:stdin_r ~stdout:stdout_w ~stderr:stderr_w prog argv in

  let stdin = (Eio_unix.FD.as_socket ~sw ~close_unix:true stdin_w :> Eio.Flow.sink) in
  let stdout = (Eio_unix.FD.as_socket ~sw ~close_unix:true stdout_r :> Eio.Flow.source) in
  let stderr = (Eio_unix.FD.as_socket ~sw ~close_unix:true stderr_r :> Eio.Flow.source) in
  (pid, { stdin; stdout; stderr })
