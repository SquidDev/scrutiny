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
  type t = unit_file

  let id x = x.u_id

  let reload x =
    let%lwt _job = Systemd_client.Org_freedesktop_systemd1_Unit.reload x.u_proxy ~mode:"replace" in
    Lwt.return_unit

  let restart x =
    let%lwt _job = Systemd_client.Org_freedesktop_systemd1_Unit.restart x.u_proxy ~mode:"replace" in
    Lwt.return_true

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
