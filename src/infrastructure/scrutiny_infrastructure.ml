include Core
include Types

type run_result = Runner.run_result = {
  total : int;
  changed : int;
  failed : int;
}

let apply ?dry_run rules =
  let (), rules = rules { Core.Rules.rules = []; user = `Current; machine = Local } in
  Lwt_switch.with_switch @@ fun switch -> Runner.apply ~switch ?dry_run rules

let run_tunnel () =
  Lwt_main.run (Lwt_switch.with_switch (fun switch -> Tunnel.run_tunnel ~switch ()))

let main = Cli.main
