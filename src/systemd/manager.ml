module StringMap = Map.Make (String)

type 'a bus_result = private
  | Ok of 'a
  | Bus_error of string * string
  | Parse_error of Unix.error * string

type unit_state_native = private {
  id : string;
  description : string;
  load_state : string;
  active_state : string;
  sub_state : string;
  following : string;
  unit_path : string;
}

type job_removed = private {
  job : string;
  result : string;
}

module Native = struct
  type sd_bus

  (** {2 Bus management} *)

  external scrutiny_sd_bus_open_user : unit -> sd_bus = "scrutiny_sd_bus_open_user"
  external scrutiny_sd_bus_open_system : unit -> sd_bus = "scrutiny_sd_bus_open_system"
  external scrutiny_sd_bus_open_machine : string -> sd_bus = "scrutiny_sd_bus_open_machine"
  external scrutiny_sd_bus_open_address : string -> sd_bus = "scrutiny_sd_bus_open_address"
  external scrutiny_sd_bus_close : sd_bus -> unit = "scrutiny_sd_bus_close"

  (** {2 Event handling} *)
  external scrutiny_sd_bus_get_fd : sd_bus -> Unix.file_descr = "scrutiny_sd_bus_get_fd"

  external scrutiny_sd_bus_process : sd_bus -> unit = "scrutiny_sd_bus_process"

  external scrutiny_sd_bus_subscribe_job_removed : sd_bus -> (job_removed -> unit) -> unit
    = "scrutiny_sd_bus_subscribe_job_removed"

  (** {2 Actual messages} *)

  (** Invoke [org.freedesktop.DBus.Properties.Get()] on an interface. *)
  external scrutiny_sd_bus_get_prop :
    sd_bus -> string -> string -> string -> (string bus_result -> unit) -> unit
    = "scrutiny_sd_bus_get_prop"

  external scrutiny_sd_bus_list_units :
    sd_bus -> (unit_state_native list bus_result -> unit) -> unit = "scrutiny_sd_bus_list_units"

  external scrutiny_sd_bus_load_unit : sd_bus -> string -> (string bus_result -> unit) -> unit
    = "scrutiny_sd_bus_load_unit"

  external scrutiny_sd_bus_call_noarg : sd_bus -> string -> (unit bus_result -> unit) -> unit
    = "scrutiny_sd_bus_call_noarg"

  external scrutiny_sd_bus_unit_action :
    sd_bus -> string -> string -> (string bus_result -> unit) -> unit
    = "scrutiny_sd_bus_unit_action"

  external scrutiny_sd_bus_enable_unit_file : sd_bus -> string -> (unit bus_result -> unit) -> unit
    = "scrutiny_sd_bus_enable_unit_file"

  external scrutiny_sd_bus_disable_unit_file : sd_bus -> string -> (unit bus_result -> unit) -> unit
    = "scrutiny_sd_bus_disable_unit_file"
end

module JobRemoval = struct
  type t =
    | UnknownId of string StringMap.t
    | KnownId of string * string Lwt.u

  let notify ({ job; result; _ } : job_removed) listener =
    match !listener with
    | UnknownId ids -> listener := UnknownId (StringMap.add job result ids)
    | KnownId (job', callback) -> if job = job' then Lwt.wakeup_later callback result

  let notify_all listeners job = Lwt_dllist.iter_l (fun l -> notify job l) listeners
end

let call_native invoke cb =
  let promise, resolve = Lwt.wait () in
  invoke (fun units -> Lwt.wakeup_later resolve units);
  let open Lwt.Infix in
  promise >|= function
  | Bus_error (msg, descr) -> failwith (Printf.sprintf "%s: %s" msg descr)
  | Parse_error (x, y) -> raise (Unix.Unix_error (x, "call_native", y))
  | Ok x -> cb x

type t = {
  bus : Native.sd_bus;
  removed_job_listeners : JobRemoval.t ref Lwt_dllist.t;
  mutable is_open : bool;
  mutable listening : unit Lwt.t option;
}

let close x =
  if not x.is_open then failwith "close: Already closed";
  x.is_open <- false;
  Native.scrutiny_sd_bus_close x.bus

let process x =
  let ofd = Native.scrutiny_sd_bus_get_fd x.bus in
  let fd = ofd |> Lwt_unix.of_unix_file_descr ~blocking:false in

  let rec go () =
    if x.is_open then
      let open Lwt.Infix in
      Lwt_unix.wait_read fd >>= fun () ->
      if x.is_open then (
        (* TODO: Use get_events to await readable or writable. *)
        Native.scrutiny_sd_bus_process x.bus;
        go ())
      else Lwt.return_unit
    else Lwt.return_unit
  in
  go ()

type connection =
  [ `User
  | `System
  | `Other_user of string
  | `Bus of string
  ]

let of_bus ~sw (conn : connection) =
  Lwt_switch.check (Some sw);
  let bus =
    match conn with
    | `User -> Native.scrutiny_sd_bus_open_user ()
    | `System -> Native.scrutiny_sd_bus_open_system ()
    | `Other_user u -> Native.scrutiny_sd_bus_open_machine (u ^ "@")
    | `Bus b -> Native.scrutiny_sd_bus_open_address b
  in
  let bus =
    { bus; is_open = true; removed_job_listeners = Lwt_dllist.create (); listening = None }
  in
  Lwt.async (fun () -> process bus);
  (* Cannot wait for eio! *)
  Lwt_switch.add_hook (Some sw) (fun () -> close bus; Lwt.return_unit);
  bus

let get_prop bus ~path ~interface ~name =
  call_native (Native.scrutiny_sd_bus_get_prop bus.bus path interface name) Fun.id

module Unit = struct
  type nonrec t = {
    bus : t;
    id : string;
    path : string;
  }

  let interface = "org.freedesktop.systemd1.Unit"
  let id (u : t) = u.id
  let l_protect ~finally f = Lwt.finalize f (fun () -> finally (); Lwt.return_unit)

  (** Sets up a job listener, runs a function which creates a job (for instance restarting a
      service) and then waits for that job to finish, returning the job's result. *)
  let job_action action ({ bus; id; _ } : t) =
    let%lwt () =
      match bus.listening with
      | Some subscribe -> subscribe
      | None ->
          Native.scrutiny_sd_bus_subscribe_job_removed bus.bus
            (JobRemoval.notify_all bus.removed_job_listeners);
          let listening =
            Lwt.protected
              (call_native (Native.scrutiny_sd_bus_call_noarg bus.bus "Subscribe") Fun.id)
          in
          bus.listening <- Some listening;
          listening
    in

    let cell = ref (JobRemoval.UnknownId StringMap.empty) in
    let node = Lwt_dllist.add_r cell bus.removed_job_listeners in

    l_protect ~finally:(fun () -> Lwt_dllist.remove node) @@ fun () ->
    let%lwt job = call_native (Native.scrutiny_sd_bus_unit_action bus.bus id action) Fun.id in
    Printf.printf "Got job %S\n%!" job;
    let%lwt result =
      match !cell with
      | KnownId _ -> failwith "Impossible."
      | UnknownId are_done -> (
        match StringMap.find_opt job are_done with
        | Some result -> Lwt.return result
        | None ->
            let wait, signal = Lwt.wait () in
            cell := KnownId (job, signal);
            wait)
    in

    Lwt_dllist.remove node;
    match result with
    | "done" -> Lwt.return_ok ()
    | result -> Lwt.return_error result

  let start = job_action "StartUnit"
  let stop = job_action "StopUnit"
  let reload = job_action "ReloadUnit"
  let restart = job_action "RestartUnit"

  let enable ({ bus; id; _ } : t) =
    call_native (Native.scrutiny_sd_bus_enable_unit_file bus.bus id) Fun.id

  let disable ({ bus; id; _ } : t) =
    call_native (Native.scrutiny_sd_bus_disable_unit_file bus.bus id) Fun.id

  let get_load_state ({ bus; path; _ } : t) = get_prop bus ~interface ~path ~name:"LoadState"
  let get_active_state ({ bus; path; _ } : t) = get_prop bus ~interface ~path ~name:"ActiveState"

  let get_unit_file_state ({ bus; path; _ } : t) =
    get_prop bus ~interface ~path ~name:"UnitFileState"
end

module Service = struct
  type t = Unit.t

  let interface = "org.freedesktop.systemd1.Service"
  let unit s = s

  let of_unit (u : Unit.t) =
    if String.ends_with ~suffix:".service" u.id then u
    else invalid_arg (Printf.sprintf "%S is not a service" u.id)

  let get_control_group ({ bus; path; _ } : t) = get_prop bus ~interface ~path ~name:"ControlGroup"
end

type unit_state = {
  id : string;
  description : string;
  load_state : string;
  active_state : string;
  sub_state : string;
  following : string;
  unit : Unit.t;
}

let list_units bus =
  call_native (Native.scrutiny_sd_bus_list_units bus.bus) @@ fun r ->
  List.rev_map
    (fun (x : unit_state_native) : unit_state ->
      {
        id = x.id;
        description = x.description;
        load_state = x.load_state;
        active_state = x.active_state;
        sub_state = x.sub_state;
        following = x.following;
        unit = { Unit.bus; id = x.id; path = x.unit_path };
      })
    r

let load_unit bus unit_name =
  call_native (Native.scrutiny_sd_bus_load_unit bus.bus unit_name) (fun path ->
      { Unit.bus; id = unit_name; path })

let daemon_reload bus = call_native (Native.scrutiny_sd_bus_call_noarg bus.bus "Reload") Fun.id
