open Eio.Std
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Infra = Scrutiny_infrastructure
module Journal = Scrutiny_systemd.Journal
module Systemd = Scrutiny_systemd.Manager
module Log = (val Logs.src_log (Logs.Src.create __MODULE__) : Logs.LOG)

let unit_fields = [ "USER_UNIT"; "UNIT"; "_SYSTEMD_USER_UNIT"; "_SYSTEMD_UNIT" ]

let watch_journal ~sw ~unit_name =
  let journal = Journal.open_ ~sw () in
  Journal.seek_tail journal;

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
    let _ = Journal.wait journal in
    read_journal (); watch_journal ()
  in
  Fiber.fork_daemon ~sw watch_journal

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

  let unit_state_matches ~current ~target =
    match current with
    | "generated" -> true
    | _ -> current = target

  (** Monitor a unit, ensuring it remains running for some period. *)
  let monitor_unit ~clock name unit_file target =
    if target.monitor <= 0 then Ok ()
    else (
      (* Monitor this service for some number of seconds, then check it's still in the correct
         state. *)
      Log.info (fun f -> f "Watching %s for %d seconds" name target.monitor);
      Eio.Time.sleep clock (float_of_int target.monitor);
      let current_state = Systemd.Unit.get_active_state unit_file in
      let target_state = if target.running then "active" else "inactive" in
      if current_state = target_state then (
        Log.info (fun f -> f "Service is still %s" current_state);
        Ok ())
      else
        Printf.sprintf "Expecting service to be %s but is %s" target_state current_state
        |> Result.error)

  let apply ~env { ServiceName.name; scope } target change : (Infra.change, string) result =
    match get_desired_state change target with
    | Error e -> Error e
    | Ok (target_state, target_state_apply) ->
        let target_unit_state, target_unit_state_apply = get_desired_unit_state target in

        let scope =
          match scope with
          | `User -> `User
          | `System -> `System
        in
        let current_state, current_unit_state =
          Switch.run @@ fun sw ->
          let systemd = Systemd.of_bus ~sw scope in
          let unit_file = Systemd.load_unit systemd name in
          (Systemd.Unit.get_active_state unit_file, Systemd.Unit.get_unit_file_state unit_file)
        in

        if
          current_state = target_state
          && unit_state_matches ~current:current_unit_state ~target:target_unit_state
        then Ok Infra.Correct
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
            Switch.run @@ fun sw ->
            watch_journal ~sw ~unit_name:name;

            let systemd = Systemd.of_bus ~sw scope in
            Systemd.daemon_reload systemd;
            let unit_file = Systemd.load_unit systemd name in
            let apply_state =
              if current_state <> target_state then target_state_apply unit_file else Ok ()
            in
            match apply_state with
            | Error e -> Error (Printf.sprintf "Failed to %s service (%s)" target_state e)
            | Ok () ->
                if not (unit_state_matches ~current:current_unit_state ~target:target_unit_state)
                then target_unit_state_apply unit_file;
                if target.running && current_state <> target_state then
                  monitor_unit ~clock:env#clock name unit_file target
                else Ok ()
          in
          Ok (Infra.NeedsChange { diff; apply })
end

let service_resource =
  Infra.Resource.make
    (module Service : Infra.Resource
      with type Key.t = ServiceName.t
       and type EdgeOptions.t = ServiceChange.t
       and type Value.t = service_state)

let service ~name ~scope action =
  let name = if String.contains name '.' then name else name ^ ".service" in
  Infra.Rules.resource service_resource { name; scope } action
