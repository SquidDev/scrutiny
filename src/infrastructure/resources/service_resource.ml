module Infra = Scrutiny_infrastructure
module Journal = Scrutiny_systemd.Journal
module Systemd = Scrutiny_systemd.Manager
module Log = (val Logs.src_log (Logs.Src.create __MODULE__) : Logs.LOG)

let unit_fields = [ "USER_UNIT"; "UNIT"; "_SYSTEMD_USER_UNIT"; "_SYSTEMD_UNIT" ]

let watch_journal ~switch ~unit_name =
  Lwt_switch.check switch;

  let journal = Journal.open_ () in
  Journal.seek_tail journal;

  let wait, signal = Lwt.wait () in
  Lwt_switch.add_hook switch (fun () -> Lwt.wakeup signal Journal.Nop; Lwt.return_unit);

  let rec this_unit = function
    | [] -> false
    | name :: xs -> (
      match Journal.get_data journal name with
      | Some x when x = unit_name -> true
      | None | Some _ -> this_unit xs)
  in
  let rec read_journal () =
    if Journal.next journal then (
      let message = Journal.get_data_exn journal "MESSAGE" |> String.trim in
      (if this_unit unit_fields then
       let log : ('a, unit) Logs.msgf = fun f -> f ~header:unit_name "%s" message in
       match Journal.get_data journal "PRIORITY" with
       | Some ("0" | "1" | "2" | "3") -> Log.err log
       | Some "4" -> Log.warn log
       | Some ("5" | "6") -> Log.info log
       | Some ("7" | _) | None -> Log.debug log);
      read_journal ())
  in
  let rec watch_journal () =
    if Option.fold ~none:true ~some:Lwt_switch.is_on switch then (
      let%lwt _ = Lwt.choose [ Journal.wait journal; wait ] in
      read_journal (); watch_journal ())
    else (Journal.close journal; Lwt.return_unit)
  in
  Lwt.async watch_journal

module ServiceName = struct
  type t = {
    name : string;
    scope : [ `System | `User ];
  }
  [@@deriving yojson]

  let hash = Hashtbl.hash
  let equal = ( = )

  let digest { name; scope } =
    let scope =
      match scope with
      | `User -> "user"
      | `System -> "system"
    in
    name ^ "@" ^ scope
end

type service_state = {
  enabled : bool;
  running : bool;
  monitor : int;
}

let service_state ?(enabled = false) ?(running = false) ?(monitor = 0) () =
  { enabled; running; monitor }

module ServiceState = struct
  type t = service_state = {
    enabled : bool;
    running : bool;
    monitor : int;
  }
  [@@deriving yojson]

  let digest state = Yojson.Safe.to_string (yojson_of_t state)
end

module ServiceChange = struct
  type t =
    [ `None
    | `Restart
    | `Reload
    ]
  [@@deriving yojson]

  let default = `None

  let union (l : t) (r : t) : t =
    match (l, r) with
    | `None, `None -> `None
    | (`None | `Reload | `Restart), `Restart | `Restart, (`None | `Reload) -> `Restart
    | (`None | `Reload), `Reload | `Reload, `None -> `Reload
end

(** Store the bus in a global state. It's not nice, but the only thing we can really do. *)
let get_bus : [ `System | `User ] -> Systemd.t =
  let sw = Lwt_switch.create () in
  let user_bus = ref None in
  let system_bus = ref None in

  let get_bus ref kind =
    match !ref with
    | Some x -> x
    | None ->
        let manager = Systemd.of_bus ~sw kind in
        ref := Some manager;
        manager
  in
  function
  | `User -> get_bus user_bus `User
  | `System -> get_bus system_bus `System

module Service = struct
  module Key = ServiceName
  module Value = ServiceState
  module EdgeOptions = ServiceChange

  let id = "service"

  let pp out { ServiceName.name; scope } =
    match scope with
    | `User -> Format.fprintf out "User service %s" name
    | `System -> Format.fprintf out "System service %s" name

  let get_desired_state (change : ServiceChange.t) target =
    match (change, target.running) with
    | `None, true -> Ok ("active", Systemd.Unit.restart)
    | `None, false -> Ok ("inactive", Systemd.Unit.stop)
    | `Reload, true -> Ok ("reload", Systemd.Unit.reload)
    | `Restart, true -> Ok ("restart", Systemd.Unit.restart)
    | (`Reload | `Restart), false -> Error "Cannot restart/reload stopped service."

  let get_desired_unit_state target =
    if target.enabled then ("enabled", Systemd.Unit.enable) else ("disabled", Systemd.Unit.disable)

  (** Monitor a unit, ensuring it remains running for some period. *)
  let monitor_unit name unit_file target =
    if target.monitor <= 0 then Lwt.return_ok ()
    else (
      (* Monitor this service for some number of seconds, then check it's still in the correct
         state. *)
      Log.info (fun f -> f "Watching %s for %d seconds" name target.monitor);
      let%lwt () = Lwt_unix.sleep (float_of_int target.monitor) in
      let%lwt current_state = Systemd.Unit.get_active_state unit_file in
      let target_state = if target.running then "active" else "inactive" in
      if current_state = target_state then (
        Log.info (fun f -> f "Service is still %s" current_state);
        Lwt.return_ok ())
      else
        Printf.sprintf "Expecting service to be %s but is %s" target_state current_state
        |> Lwt.return_error)

  let apply { ServiceName.name; scope } target change : (Infra.change, string) result Lwt.t =
    match get_desired_state change target with
    | Error e -> Lwt.return_error e
    | Ok (target_state, target_state_apply) ->
        let target_unit_state, target_unit_state_apply = get_desired_unit_state target in

        let systemd = get_bus scope in
        let%lwt unit_file = Systemd.load_unit systemd name in
        let%lwt current_state = Systemd.Unit.get_active_state unit_file
        and current_unit_state = Systemd.Unit.get_unit_file_state unit_file in

        if current_state = target_state && current_unit_state = target_unit_state then
          Lwt.return_ok Infra.Correct
        else
          let diff =
            let open Scrutiny_diff in
            structure
              [
                ("enabled", of_line ~old:current_unit_state ~new_:target_unit_state);
                ("active", of_line ~old:current_state ~new_:target_state);
              ]
          in

          (* TODO: Refactor this somewhere which isn't /quite/ so ugly? There's a lot of captured
             state though, which makes this a little more awkward. *)
          let apply () =
            Lwt_switch.with_switch @@ fun sw ->
            watch_journal ~switch:(Some sw) ~unit_name:name;

            let%lwt () = Systemd.daemon_reload systemd in
            let%lwt apply_state =
              if current_state <> target_state then target_state_apply unit_file
              else Lwt.return_ok ()
            in
            match apply_state with
            | Error e ->
                Lwt.return_error (Printf.sprintf "Failed to %s service (%s)" target_state e)
            | Ok () ->
                let%lwt () =
                  if current_unit_state <> target_unit_state then target_unit_state_apply unit_file
                  else Lwt.return_unit
                in
                if target.running && current_state <> target_state then
                  monitor_unit name unit_file target
                else Lwt.return_ok ()
          in
          Lwt.return_ok (Infra.NeedsChange { diff; apply })
end

let service_module =
  Infra.Resource.make
    (module Service : Infra.Resource
      with type Key.t = ServiceName.t
       and type EdgeOptions.t = ServiceChange.t
       and type Value.t = service_state)

let service ~name ~scope action =
  let name = if String.contains name '.' then name else name ^ ".service" in
  Infra.Rules.resource service_module { name; scope } @@ fun () ->
  let open Infra.Action in
  let+ result = action () in
  Lwt.return result
