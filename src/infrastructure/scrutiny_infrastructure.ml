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
  Lwt_switch.with_switch @@ fun switch -> Runner.apply ~env ~switch ?dry_run rules

let run_tunnel () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _token ->
  Lwt_eio.run_lwt @@ fun () ->
  Lwt_switch.with_switch (fun switch -> Tunnel.run_tunnel ~env ~switch ())

let main = Cli.main
