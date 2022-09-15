open Core
module ITbl = Hashtbl.Make (CCInt)
module STbl = Hashtbl.Make (CCString)
module PTbl = Hashtbl.Make (Executor.PartialKey)
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

type user =
  [ `Current
  | `Id of int
  | `Name of string
  ]
[@@deriving yojson]

module Diff = struct
  type t = Scrutiny_diff.t

  let yojson_of_t x : Yojson.Safe.t = `String (Marshal.to_string x [])

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `String x -> Marshal.from_string x 0
    | _ -> raise (Yojson.Json_error "Malformed Scrutiny_diff")
end

let pp_exception out = function
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, json) ->
      Fmt.pf out "%a in %a" Fmt.exn e (Yojson.Safe.pretty_print ~std:false) json
  | e -> Fmt.exn out e

module AppliedResource = struct
  type ('key, 'value, 'options) t = {
    resource : ('key, 'value, 'options) Resource.t;
    key : 'key;
    value : 'value;
    options : 'options;
    user : user;
  }

  type boxed = Boxed : ('key, 'value, 'options) t -> boxed

  let boxed_of_yojson : Yojson.Safe.t -> boxed = function
    | `Assoc
        [
          ("resource", `String resource);
          ("key", key);
          ("value", value);
          ("options", options);
          ("user", user);
        ] ->
        let (Resource.Boxed resource) =
          match SMap.find_opt resource !Resource.resources with
          | None -> raise (Yojson.Json_error (Printf.sprintf "Unknown resource %S" resource))
          | Some x -> x
        in
        let module R = (val resource) in
        Boxed
          {
            resource;
            key = R.Key.t_of_yojson key;
            value = R.Value.t_of_yojson value;
            options = R.EdgeOptions.t_of_yojson options;
            user = user_of_yojson user;
          }
    | _ -> raise (Yojson.Json_error "Malformed AppliedResource")

  let yojson_of_boxed (Boxed { resource; key; value; options; user }) : Yojson.Safe.t =
    let module R = (val resource) in
    `Assoc
      [
        ("resource", `String R.id);
        ("key", R.Key.yojson_of_t key);
        ("value", R.Value.yojson_of_t value);
        ("options", R.EdgeOptions.yojson_of_t options);
        ("user", yojson_of_user user);
      ]
end

module Logging = struct
  type level = Logs.level =
    | App
    | Error
    | Warning
    | Info
    | Debug
  [@@deriving yojson]

  type record = {
    level : level;
    src : string;
    message : string;
    header : string option;
    key : int option; [@yojson.default None]
  }
  [@@deriving yojson]

  let reporter keys send =
    let report (type a b) src level ~over k (msg : (a, b) Logs.msgf) : b =
      msg @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
      let key =
        match Logs.Tag.find Executor.PartialKey.tag tags with
        | None -> None
        | Some key -> PTbl.find_opt keys key
      in
      Format.kasprintf
        (fun message ->
          send { header; level; src = Logs.Src.name src; message; key };
          over ();
          k ())
        fmt
    in
    { Logs.report }

  let log_record keys { level; src = _; message; header; key } =
    Log.msg level (fun f ->
        let tags = Logs.Tag.empty in
        let tags =
          match Option.bind key (ITbl.find_opt keys) with
          | None -> tags
          | Some key -> Logs.Tag.add Executor.PartialKey.tag key tags
        in
        f ~tags ?header "%s" message)
end

type to_server =
  | Check of {
      id : int;
      resource : AppliedResource.boxed;
    }
  | Apply of { id : int }
[@@deriving yojson]

type to_client =
  | Init of {
      message : string;
      version : string;
    }
  | CheckResult of {
      id : int;
      result : [ `Correct | `NeedsChange of Diff.t ] Or_exn.t;
    }
  | ApplyResult of {
      id : int;
      result : unit Or_exn.t;
    }
  | Log of Logging.record
[@@deriving yojson]

(** Build a cached lookup of users to specific uids. *)
let make_user_lookup () =
  let current_user = Unix.getuid () in
  let user_ids = STbl.create 4 in
  let get_user_id : Core.user -> (int, string) result Lwt.t = function
    | `Current -> Lwt.return_ok current_user
    | `Id x -> Lwt.return_ok x
    | `Name name -> (
      match STbl.find_opt user_ids name with
      | Some x -> Lwt.return x
      | None ->
          let%lwt res =
            match%lwt Lwt_unix.getpwnam name with
            | { pw_uid; _ } -> Lwt.return_ok pw_uid
            | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                Lwt.return_error (Printf.sprintf "User %S not found" name)
          in
          STbl.replace user_ids name res; Lwt.return res)
  in
  get_user_id

type channels = {
  stdin : Lwt_io.output_channel;
  stdout : Lwt_io.input_channel;
  stderr : Lwt_io.input_channel;
}

let wait_for_init (proc : channels) =
  let rec gobble () =
    match%lwt Lwt_io.read_line proc.stdout with
    | input -> (
      match Yojson.Safe.from_string input |> to_client_of_yojson with
      | Init { version; _ } when version = "%VERSION%" -> Lwt.return_ok ()
      | Init { version; _ } ->
          Printf.sprintf "Tunnel version %s is incompatible with current version %s" version
            "%VERSION%"
          |> Lwt.return_error
      | _ -> Lwt.return_error "Unexpected other message"
      | exception Yojson.Json_error _ -> gobble ())
    | exception End_of_file -> Lwt.return_error "Unexpected end of file"
  in
  try%lwt Lwt_unix.with_timeout 5.0 gobble
  with Lwt_unix.Timeout -> Lwt.return_error "Tunnel failed after a timeout."

let rec drain ?switch (proc : channels) =
  match%lwt Lwt_io.read_line proc.stderr with
  | exception End_of_file -> Lwt.return_unit
  | exception Lwt_io.Channel_closed _ -> Lwt.return_unit
  | line ->
      Log.err (fun e -> e "%s" line);
      if Option.fold ~none:true ~some:Lwt_switch.is_on switch then drain ?switch proc
      else Lwt.return_unit

let executor ?switch ~input ~output () : Executor.t =
  let counter = ref 0 in
  let check_wait = ITbl.create 32 in
  let apply_wait = ITbl.create 32 in
  let keys = ITbl.create 32 in
  Lwt_switch.check switch;

  let background =
    let rec go () =
      match%lwt Lwt_io.read_line input with
      | line ->
          (match Yojson.Safe.from_string line |> to_client_of_yojson with
          | Init _ -> Log.err (fun f -> f "Impossible: Additional init message")
          | CheckResult { id; result } -> Lwt.wakeup_later (ITbl.find check_wait id) result
          | ApplyResult { id; result } -> Lwt.wakeup_later (ITbl.find apply_wait id) result
          | Log msg -> Logging.log_record keys msg
          | exception e -> Log.err (fun f -> f "Error reading tunnel message (%a)" pp_exception e));
          go ()
      | exception End_of_file -> Lwt.return_unit
    in
    go ()
  in
  Lwt_switch.add_hook switch (fun () -> Lwt.cancel background; Lwt.return_unit);

  let apply id =
    Lwt_switch.check switch;

    let wait, notify = Lwt.wait () in
    ITbl.replace apply_wait id notify;

    let message = Apply { id } |> yojson_of_to_server |> Yojson.Safe.to_string in
    let%lwt () = Lwt_io.atomic (fun out -> Lwt_io.write_line out message) output in
    wait
  in

  let check ~user resource key value options =
    Lwt_switch.check switch;

    let resource_box = AppliedResource.Boxed { resource; key; value; options; user } in
    let id = !counter in
    incr counter;
    ITbl.replace keys id (PKey (resource, key));

    let wait, notify = Lwt.wait () in
    ITbl.replace check_wait id notify;

    let message =
      Check { id; resource = resource_box } |> yojson_of_to_server |> Yojson.Safe.to_string
    in
    let%lwt () = Lwt_io.atomic (fun out -> Lwt_io.write_line out message) output in
    let%lwt result = wait in
    let result =
      result
      |> Or_exn.map @@ function
         | `Correct -> Executor.ECorrect
         | `NeedsChange diff -> Executor.ENeedsChange { diff; apply = (fun () -> apply id) }
    in
    Lwt.return result
  in
  { Executor.apply = check }

let executor_of_cmd ?switch setup cmd =
  Lwt_switch.check switch;

  let stdin_r, stdin_w = Unix.pipe ~cloexec:true ()
  and stdout_r, stdout_w = Unix.pipe ~cloexec:true ()
  and stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  let proc = Unix.create_process cmd.(0) cmd stdin_r stdout_w stderr_w in

  let channels =
    {
      stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w;
      stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r;
      stderr = Lwt_io.of_unix_fd ~mode:Lwt_io.input stderr_r;
    }
  in
  let close () =
    let%lwt () =
      Lwt.protected
        (Lwt.join
           [
             Lwt_io.close channels.stdin; Lwt_io.close channels.stdout; Lwt_io.close channels.stderr;
           ])
    in
    Lwt_unix.wait4 [] proc
  in

  Lwt_switch.add_hook switch (fun () ->
      let%lwt _ = close () in
      Lwt.return_unit);
  Lwt.async (fun () -> drain ?switch channels);
  match%lwt setup channels with
  | Ok () -> Lwt.return_ok (executor ?switch ~input:channels.stdout ~output:channels.stdin ())
  | Error e -> Lwt.return_error e

let make_executor_factory ?switch () : Core.user -> Executor.t Or_exn.t Lwt.t =
  Lwt_switch.check switch;

  let current_user = Unix.getuid () in
  let find_user = make_user_lookup () in
  let user_tunnels = ITbl.create 4 in

  let create uid =
    let%lwt user = Lwt_unix.getpwuid uid in
    Log.info (fun f -> f "Opening tunnel for %S (%d)" user.Unix.pw_name uid);
    executor_of_cmd ?switch wait_for_init
      [| "sudo"; "--user"; user.Unix.pw_name; Sys.executable_name |]
  in
  let memo user =
    match ITbl.find_opt user_tunnels user with
    | Some x -> x
    | None ->
        let tunnel = Or_exn.run_lwt (fun () -> create user) in
        ITbl.replace user_tunnels user tunnel;
        tunnel
  in
  let find user : Executor.t Or_exn.t Lwt.t =
    match%lwt find_user user with
    | Error msg -> Lwt.return (Or_exn.Error msg)
    | Ok user ->
        if user = current_user then Lwt.return (Or_exn.Ok Executor.local)
        else if current_user <> 0 then
          Lwt.return (Or_exn.Error "Cannot use different users when not root")
        else memo user
  in
  find

let ssh ?switch ({ sudo_pw; host; tunnel_path } : Remote.t) =
  Lwt_switch.check switch;
  let setup, cmd =
    let ssh = Sys.getenv_opt "SCRUTINY_SSH" |> Option.value ~default:"ssh" in
    match sudo_pw with
    | None -> (wait_for_init, [| ssh; host; tunnel_path |])
    | Some password ->
        let prompt proc =
          let%lwt () = Lwt_unix.sleep 0.5 (* TODO: Wait for prompt rather than sleeping. *) in
          let%lwt () = Lwt_io.write_line proc.stdin password in
          wait_for_init proc
        in
        (prompt, [| ssh; host; "sudo"; "-S"; "-p"; "sudo[scrutiny-infra-tunnel]: "; tunnel_path |])
  in
  executor_of_cmd ?switch setup cmd

let run_tunnel ?switch () : unit Lwt.t =
  let pending_actions = ITbl.create 32 in
  let keys = PTbl.create 32 in

  let run_as_user = make_executor_factory ?switch () in

  let send msg =
    let result = yojson_of_to_client msg |> Yojson.Safe.to_string in
    Lwt_io.atomic (fun out -> Lwt_io.write_line out result) Lwt_io.stdout
  in
  let check_resource id (AppliedResource.Boxed { resource; key; value; options; user }) =
    let%lwt result =
      match%lwt run_as_user user with
      | Error msg -> Lwt.return (Or_exn.Error msg)
      | Exception msg -> Lwt.return (Or_exn.Exception msg)
      | Ok executor -> executor.apply ~user:`Current resource key value options
    in
    let result =
      result
      |> Or_exn.map @@ function
         | Executor.ENeedsChange { diff; apply } ->
             ITbl.replace pending_actions id apply;
             `NeedsChange diff
         | Executor.ECorrect -> `Correct
    in
    CheckResult { id; result } |> send
  in

  let apply_resource id apply =
    let%lwt result = apply () in
    ApplyResult { id; result } |> send
  in

  let handle_message = function
    | Check { id; resource } ->
        if ITbl.mem pending_actions id then failwith "Duplicate resource";
        Lwt.async (fun () -> check_resource id resource)
    | Apply { id } -> (
      match ITbl.find_opt pending_actions id with
      | None -> failwith "No such resource"
      | Some action -> Lwt.async (fun () -> apply_resource id action))
  in
  let rec run () =
    match%lwt Lwt_io.read_line Lwt_io.stdin with
    | input ->
        Yojson.Safe.from_string input |> to_server_of_yojson |> handle_message;
        run ()
    | exception End_of_file -> Lwt.return_unit
  in

  Printexc.record_backtrace true;

  (if Option.is_none (Sys.getenv_opt "XDG_RUNTIME_DIR") then
   let id = Unix.getuid () in
   if id <> 0 then Unix.putenv "XDG_RUNTIME_DIR" (Printf.sprintf "/run/user/%d" id));

  (* We print an "init" message which contains the version information. *)
  let%lwt () =
    Init
      { message = "Running scrutiny-infra-tunnel, waiting for a command."; version = "%VERSION%" }
    |> send
  in

  (* Set up a logging reporter. *)
  Logs.set_level ~all:true (Some Debug);
  Logging.reporter keys (fun msg -> Lwt.async (fun () -> Log msg |> send))
  |> Executor.wrap_logger |> Logs.set_reporter;
  run ()
