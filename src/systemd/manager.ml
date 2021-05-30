type manager =
  { m_bus : OBus_bus.t;
    m_proxy : OBus_proxy.t;
    mutable m_listening : bool
  }

type unit_file =
  { u_manager : manager;
    u_id : string;
    u_proxy : OBus_proxy.t
  }

module Unit = struct
  module SMap = Map.Make (String)

  type job_progress =
    | WaitingFor of (string * string Lwt.u)
    | IsDone of string SMap.t

  type t = unit_file

  let id x = x.u_id

  (** Sets up a job listener, runs a function which creates a job (for instance restarting a
      service) and then waits for that job to finish, returning the job's result. *)
  let watching_job fn x =
    let manager = x.u_manager in
    let%lwt () =
      if manager.m_listening then Lwt.return_unit
      else Systemd_client.Org_freedesktop_systemd1_Manager.subscribe manager.m_proxy
    in
    Lwt_switch.with_switch @@ fun switch ->
    let cell = ref (IsDone SMap.empty) in

    let%lwt event =
      Systemd_client.Org_freedesktop_systemd1_Manager.job_removed manager.m_proxy
      |> OBus_signal.connect ~switch
    in
    let watcher =
      event
      |> React.E.map @@ fun (_, path, _, result) ->
         let path = OBus_proxy.path path |> OBus_path.to_string in
         match !cell with
         | IsDone are_done -> cell := IsDone (SMap.add path result are_done)
         | WaitingFor (needs, finished) -> if needs = path then Lwt.wakeup_later finished result
    in

    let%lwt job = fn x in
    let job = OBus_proxy.path job |> OBus_path.to_string in
    let%lwt result =
      match !cell with
      | WaitingFor _ -> failwith "Impossible."
      | IsDone are_done -> (
        match SMap.find_opt job are_done with
        | Some result -> Lwt.return result
        | None ->
            let wait, signal = Lwt.wait () in
            cell := WaitingFor (job, signal);
            wait)
    in

    (* While this may not be needed, it means we ensure watcher remains in scope. *)
    React.E.stop watcher;
    match result with
    | "done" -> Lwt.return_ok ()
    | result -> Lwt.return_error result

  let start =
    watching_job (fun x ->
        Systemd_client.Org_freedesktop_systemd1_Unit.start x.u_proxy ~mode:"replace")

  let stop =
    watching_job (fun x ->
        Systemd_client.Org_freedesktop_systemd1_Unit.stop x.u_proxy ~mode:"replace")

  let reload =
    watching_job (fun x ->
        Systemd_client.Org_freedesktop_systemd1_Unit.reload x.u_proxy ~mode:"replace")

  let restart =
    watching_job (fun x ->
        Systemd_client.Org_freedesktop_systemd1_Unit.restart x.u_proxy ~mode:"replace")

  let enable x =
    (* TODO: Do some sanity checks here! *)
    let%lwt _ =
      Systemd_client.Org_freedesktop_systemd1_Manager.enable_unit_files x.u_manager.m_proxy
        ~files:[ x.u_id ] ~runtime:false ~force:false
    in
    Lwt.return_unit

  let disable x =
    let%lwt _ =
      Systemd_client.Org_freedesktop_systemd1_Manager.disable_unit_files x.u_manager.m_proxy
        ~files:[ x.u_id ] ~runtime:false
    in
    Lwt.return_unit

  let load_state x =
    Systemd_client.Org_freedesktop_systemd1_Unit.load_state x.u_proxy |> OBus_property.get

  let active_state x =
    Systemd_client.Org_freedesktop_systemd1_Unit.active_state x.u_proxy |> OBus_property.get

  let unit_file_state x =
    Systemd_client.Org_freedesktop_systemd1_Unit.unit_file_state x.u_proxy |> OBus_property.get
end

module Service = struct
  type t = unit_file

  let unit x = x

  let of_unit x = x

  let control_group x =
    Systemd_client.Org_freedesktop_systemd1_Service.control_group x.u_proxy |> OBus_property.get
end

type unit_state =
  { id : string;
    description : string;
    load_state : string;
    active_state : string;
    sub_state : string;
    following : string;
    unit_path : Unit.t
  }

let convert_unit ~manager
    ( id,
      description,
      load_state,
      active_state,
      sub_state,
      following,
      unit_path,
      _job_id,
      _job_type,
      _job_path ) =
  let unit_path = { u_id = id; u_manager = manager; u_proxy = unit_path } in
  { id; description; load_state; active_state; sub_state; following; unit_path }

let list_units x =
  let%lwt units = Systemd_client.Org_freedesktop_systemd1_Manager.list_units x.m_proxy in
  Lwt.return (List.map (convert_unit ~manager:x) units)

let load_unit x name =
  let%lwt proxy = Systemd_client.Org_freedesktop_systemd1_Manager.load_unit x.m_proxy ~name in
  Lwt.return { u_id = name; u_manager = x; u_proxy = proxy }

let daemon_reload x = Systemd_client.Org_freedesktop_systemd1_Manager.reload x.m_proxy

let of_bus bus =
  let peer = OBus_peer.make ~name:"org.freedesktop.systemd1" ~connection:bus in
  let proxy = OBus_proxy.make ~peer ~path:[ "org"; "freedesktop"; "systemd1" ] in
  { m_bus = bus; m_proxy = proxy; m_listening = false }

type t = manager
