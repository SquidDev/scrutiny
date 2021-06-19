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
  Logs.set_reporter (Logs_fmt.reporter ());
  let open Cmdliner in
  let ( let+ ) x f = Term.(const f $ x) in
  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b) in

  let doc = "Applies a state specified by a file." in
  let term : unit Term.t =
    let+ dry_run =
      let open Arg in
      value @@ flag
      @@ info
           ~doc:"Do not actually apply these states, only printing out what would have changed."
           [ "dry-run"; "d" ]
    and+ verbose =
      let open Arg in
      value @@ flag @@ info ~doc:"Print more verbose log messages." [ "verbose"; "v" ]
    in
    Logs.set_level ~all:true (Some (if verbose then Debug else Info));
    Lwt_main.run (apply ?executor ~dry_run rules)
  in
  Term.exit @@ Term.eval (term, Term.info Sys.executable_name ~doc)
