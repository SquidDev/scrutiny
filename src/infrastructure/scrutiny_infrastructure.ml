include Core
include Types

type run_result = Runner.run_result = {
  total : int;
  changed : int;
  failed : int;
}

let apply ~env ?dry_run rule_def =
  let rules = Builder_map.create 16 in
  rule_def { Rules.rules; context = { user = `Current; machine = Local } };
  Runner.apply ~env ?dry_run rules

let run_tunnel () =
  Cli.setup_logs ~extra_level:Debug Debug;
  Eio_main.run @@ fun env -> Tunnel.run_tunnel ~env ()

let main = Cli.main
