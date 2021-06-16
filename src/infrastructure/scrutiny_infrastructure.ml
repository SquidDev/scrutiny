include Core
include Types

module Executor = struct
  type t = Lwt_switch.t option -> Executor.t Lwt.t

  let local : t = fun _ -> Lwt.return Executor.local

  let run_tunnel () = Lwt_switch.with_switch (fun switch -> Tunnel.run_tunnel ~switch ())

  let ssh ?sudo_pw ~host () : t =
   fun switch ->
    Lwt_switch.check switch;
    let setup, cmd =
      match sudo_pw with
      | None -> (Tunnel.wait_for_init, [| "ssh"; host; "~/.local/bin/scrutiny-infra-tunnel" |])
      | Some password ->
          let prompt proc =
            let%lwt () = Lwt_unix.sleep 0.5 (* TODO: Wait for prompt rather than sleeping. *) in
            let%lwt () = Lwt_io.write_line proc#stdin password in
            Tunnel.wait_for_init proc
          in
          ( prompt,
            [| "ssh";
               host;
               "sudo";
               "-S";
               "-p";
               "sudo[scrutiny-infra-tunnel]: ";
               "~/.local/bin/scrutiny-infra-tunnel"
            |] )
    in
    match%lwt Tunnel.executor_of_cmd ?switch setup cmd with
    | Ok x -> Lwt.return x
    | Error e -> failwith e
end

let apply ?(executor = Executor.local) ?dry_run rules : unit Lwt.t =
  Lwt_switch.with_switch @@ fun switch ->
  let%lwt executor = executor (Some switch) in
  Runner.apply ~executor ?dry_run rules

let main ?executor rules =
  Printexc.record_backtrace true;
  Fmt_tty.setup_std_outputs ();
  Logs.set_level ~all:true (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let open Cmdliner in
  let ( let+ ) x f = Term.(const f $ x) in

  let doc = "Applies a state specified by a file." in
  let term : unit Term.t =
    let+ dry_run = Arg.(value @@ vflag false [ (true, info [ "dry-run"; "-d" ]) ]) in
    Lwt_main.run (apply ?executor ~dry_run rules)
  in
  Term.exit @@ Term.eval (term, Term.info Sys.executable_name ~doc)
