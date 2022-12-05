(** A Prometheus exporter for SystemD services. *)

open Cohttp_eio
module Log = (val Logs.src_log (Logs.Src.create "systemd_exporter"))

let handle_prometheus snapshot =
  let response =
    Http.Response.make ~status:`OK
      ~headers:(Http.Header.of_list [ ("content-type", "text/plain; version=0.0.4") ])
      ()
  in
  let body = Format.asprintf "%a" Scrutiny_prometheus.TextFormat_0_0_4.output snapshot in

  (response, Body.Fixed body)

let app ~fs ~config (req, _reader, _client_addr) =
  match (Http.Request.resource req, Http.Request.meth req) with
  | "/metrics", `GET -> (
      let metrics =
        try Some (Metrics.collect ~fs config)
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Log.err (fun f -> f "Error gathering metrics: %a" Fmt.exn_backtrace (exn, bt));
          None
      in
      match metrics with
      | Some () -> Prometheus.CollectorRegistry.(collect default) |> handle_prometheus
      | None -> Server.internal_server_error_response)
  | "/metrics", _ -> (Http.Response.make ~status:`Method_not_allowed (), Body.Empty)
  | _ -> Server.not_found_response

let main ~busses ~cgroup ~addr ~port =
  Eio_main.run @@ fun env ->
  let config =
    {
      Metrics.busses;
      cgroups = Fpath.v cgroup |> Cgroups.get |> Result.fold ~ok:Fun.id ~error:failwith;
    }
  in
  Log.info (fun f -> f "Listening on %s:%d" addr port);
  Server.run ~port env (app ~fs:env#fs ~config)

let () =
  let term =
    let open Cmdliner.Arg in
    let ( let$ ) x f = Cmdliner.Term.(const f $ x) in
    let ( and$ ) x y = Cmdliner.Term.(const (fun x y -> (x, y)) $ x $ y) in

    let$ verbose = value & flag & info ~doc:"Show all logging messages." [ "verbose" ]
    and$ port = value & opt int 8080 & info ~doc:"Port to expose /metrics on." [ "p"; "port" ]
    and$ addr =
      value & opt string "127.0.0.1" & info ~doc:"Address to expose /metrics on." [ "a"; "addr" ]
    and$ cgroup =
      value & opt dir "/sys/fs/cgroup/" & info ~doc:"CGroup root." [ "C"; "cgroup-root" ]
    and$ system_bus =
      value & flag
      & info ~doc:"Monitor the system bus. Defaults to true if no other busses are supplied."
          [ "system-bus" ]
    and$ user_busses =
      value
      & opt_all ~vopt:None (some string) []
      & info
          ~doc:
            "User busses to listen to. If given without an argument, the current user's bus is \
             listened to."
          [ "user-bus" ]
    and$ busses =
      value & opt_all string []
      & info ~doc:"Full bus connection strings to listen to." [ "bus-path" ]
    in
    let busses : Scrutiny_systemd.Manager.connection list =
      match (system_bus, user_busses, busses) with
      | false, [], [] -> [ `System ]
      | _, _, _ ->
          (if system_bus then [ `System ] else [])
          @ List.map (Option.fold ~none:`User ~some:(fun x -> `Other_user x)) user_busses
          @ List.map (fun x -> `Bus x) busses
    in

    Printexc.record_backtrace true;
    Logs.set_level (Some (if verbose then Debug else Info));
    Logs.set_reporter (Logs.format_reporter ());
    (* Shut up cgroups logger - that's pretty noisy outside of debugging.*)
    if not verbose then
      Logs.Src.list ()
      |> List.iter (fun src ->
             if Logs.Src.name src = "scrutiny.cgroups" then Logs.Src.set_level src (Some Logs.Error));
    main ~busses ~cgroup ~addr ~port
  in

  let open Cmdliner in
  Cmd.v (Cmd.info Sys.executable_name) term |> Cmd.eval |> exit
