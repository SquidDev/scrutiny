open Lwt.Syntax
open Httpaf
open Httpaf_lwt_unix
module Log = (val Logs.src_log (Logs.Src.create "systemd_exporter"))

let respond_error reqd status reason =
  let headers = Headers.of_list [ ("connection", "close") ] in
  Reqd.respond_with_string reqd (Response.create ~headers status) reason

let request_handler config (_ : Unix.sockaddr) { Gluten.reqd; _ } =
  let request = Reqd.request reqd in
  match request with
  | { target = "/metrics"; _ } ->
      Lwt.async @@ fun () ->
      Lwt.try_bind
        (fun () -> Metrics.collect config)
        (fun () ->
          Scrutiny_prometheus_httpaf.handle_default reqd request;
          Lwt.return_unit)
        (fun exn ->
          let bt = Printexc.get_raw_backtrace () in
          Log.err (fun f -> f "Error gathering metrics: %a" Fmt.exn_backtrace (exn, bt));
          respond_error reqd `Internal_server_error "Internal Server Error";
          Lwt.return_unit)
  | _ -> respond_error reqd `Not_found "Not Found"

let error_handler (_ : Unix.sockaddr) ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
      Log.err (fun f -> f "Error handling request: %a" Fmt.exn exn);
      Body.write_string response_body "Internal error"
  | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error));
  Body.close_writer response_body

let main ~busses ~cgroup ~addr ~port =
  let catching ~tag f =
    Lwt.catch f (fun e ->
        let bt = Printexc.get_raw_backtrace () in
        Log.err (fun f -> f "Error in %s: %a" tag Fmt.exn_backtrace (e, bt));
        Printexc.raise_with_backtrace e bt)
  in
  let listen_address = Unix.(ADDR_INET (inet_addr_of_string addr, port)) in
  let config =
    {
      Metrics.busses;
      cgroups = Fpath.v cgroup |> Cgroups.get |> Result.fold ~ok:Fun.id ~error:failwith;
    }
  in
  let* _server =
    catching ~tag:"Server" @@ fun () ->
    Lwt_io.establish_server_with_client_socket listen_address
      (Server.create_connection_handler ~request_handler:(request_handler config) ~error_handler)
  in
  Log.info (fun f -> f "Listening on %s:%d" addr port);
  let forever, _ = Lwt.wait () in
  forever

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
    Lwt_main.run (main ~busses ~cgroup ~addr ~port)
  in

  let open Cmdliner in
  Cmd.v (Cmd.info Sys.executable_name) term |> Cmd.eval |> exit
