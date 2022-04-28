include Core
include Types

let apply_ ?progress ?dry_run rules : unit Lwt.t =
  Lwt_switch.with_switch @@ fun switch -> Runner.apply ?progress ~switch ?dry_run rules

let apply ?dry_run rules =
  let (), rules = rules { Core.Rules.rules = []; user = `Current; machine = Local } in
  apply_ ?dry_run rules

let main rules =
  Printexc.record_backtrace true;
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Progress.logs_reporter ());
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

    let (), rules = rules { Core.Rules.rules = []; user = `Current; machine = Local } in

    let bar ~total =
      let open Progress.Line in
      let open Progress.Color in
      list
        [
          spinner ~color:(ansi (`bright `green)) ();
          bar ~color:(ansi (`bright `cyan)) ~style:`UTF8 total;
          parens (elapsed ());
          brackets (count_to total);
        ]
    in
    Progress.with_reporter (bar ~total:(List.length rules)) @@ fun progress ->
    Lwt_main.run (apply_ ~progress ~dry_run rules)
  in
  Cmd.v (Cmd.info Sys.executable_name ~doc) term |> Cmd.eval |> exit

let run_tunnel () =
  Lwt_main.run (Lwt_switch.with_switch (fun switch -> Tunnel.run_tunnel ~switch ()))
