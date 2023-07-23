open Eio.Std
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Core
module ITbl = Hashtbl.Make (CCInt)
module STbl = Hashtbl.Make (CCString)
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

type check_result = [ `Correct | `NeedsChange of int * Diff.t ] Or_exn.t [@@deriving yojson]

module Signatures = struct
  open Scrutiny_rpc.Method

  let applied_resource = value AppliedResource.boxed_of_yojson AppliedResource.yojson_of_boxed
  let check_result = value check_result_of_yojson yojson_of_check_result

  (** Check a resource is up-to-date. *)
  let check_resource = make "scrutiny/check" (applied_resource @-> returning check_result)

  (** Apply a resource *)
  let apply_resource =
    make "scrutiny/apply"
      (int
      @-> returning (value (Or_exn.t_of_yojson unit_of_yojson) (Or_exn.yojson_of_t yojson_of_unit))
      )
end

(** The prefix for our magic version string *)
let magic_prefix = ";;scrutiny-infra-tunnel:"

(** Our current version. *)
let our_version = "%VERSION%"

(** Build a cached lookup of users to specific uids. *)
let make_user_lookup () =
  let current_user = Unix.getuid () in
  let user_ids = STbl.create 4 in
  let get_user_id : Core.user -> (int, string) result = function
    | `Current -> Ok current_user
    | `Id x -> Ok x
    | `Name name -> (
      match STbl.find_opt user_ids name with
      | Some x -> x
      | None ->
          let res =
            match Eio_unix_async.getpwnam name with
            | { pw_uid; _ } -> Ok pw_uid
            | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                Error (Printf.sprintf "User %S not found" name)
          in
          STbl.replace user_ids name res; res)
  in
  get_user_id

let wait_for_init ~clock ~input ~output:_ =
  let rec gobble () =
    match Eio.Buf_read.line input with
    | input -> (
      match CCString.chop_prefix ~pre:magic_prefix input with
      | None ->
          Log.warn (fun f -> f "Ignoring unexpected input %s" input);
          gobble ()
      | Some version ->
          if version = our_version then Ok ()
          else
            Printf.sprintf "Tunnel version %s is incompatible with current version %s" version
              our_version
            |> Result.error)
    | exception End_of_file -> Error "Unexpected end of file"
  in
  try Eio.Time.with_timeout_exn clock 5.0 gobble
  with Eio.Time.Timeout -> Error "Tunnel failed after a timeout."

let drain stderr =
  let stderr = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stderr in
  let rec go () =
    match Eio.Buf_read.line stderr with
    | exception End_of_file -> ()
    | line ->
        Log.err (fun e -> e "%s" line);
        go ()
  in
  go ()

let executor ~sw ~input ~output () : Executor.t =
  let rpc = Scrutiny_rpc.create ~sw [] input output in

  let apply (id : int) = Scrutiny_rpc.call rpc Signatures.apply_resource id in

  let check ~user resource key value options =
    let resource_box = AppliedResource.Boxed { resource; key; value; options; user } in
    Scrutiny_rpc.call rpc Signatures.check_resource resource_box
    |> Or_exn.map @@ function
       | `Correct -> Executor.ECorrect
       | `NeedsChange (id, diff) -> Executor.ENeedsChange { diff; apply = (fun () -> apply id) }
  in
  { Executor.apply = check }

let executor_of_cmd ~mgr ~outer_sw ~sw setup cmd =
  Switch.check sw;

  (* We use two nested switches here. stdin is managed to the inner switch, which is closed first.
     The outer switch is the one which runs the process, waiting for it to finish. *)
  let stdin_r, stdin_w = Eio.Process.pipe ~sw mgr
  and stdout_r, stdout_w = Eio.Process.pipe ~sw:outer_sw mgr
  and stderr_r, stderr_w = Eio.Process.pipe ~sw:outer_sw mgr in
  let proc =
    Eio.Process.spawn ~sw:outer_sw mgr ~stdin:stdin_r ~stdout:stdout_w ~stderr:stderr_w cmd
  in
  Fiber.fork_daemon ~sw (fun () -> drain stderr_r; `Stop_daemon);
  Fiber.fork ~sw:outer_sw (fun () -> Eio.Process.await_exn proc);

  let input = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) stdout_r in
  let output = (stdin_w :> Eio.Flow.sink) in
  match Eio.Buf_write.with_flow output @@ fun output -> setup ~input ~output with
  | Ok () -> Ok (executor ~sw ~input ~output ())
  | Error e -> Error e

let make_executor_factory ~env ~outer_sw ~sw () : Core.user -> Executor.t Or_exn.t =
  let current_user = Unix.getuid () in
  let find_user = make_user_lookup () in
  let user_tunnels = ITbl.create 4 in

  let create uid =
    let user = Eio_unix_async.getpwuid uid in
    Log.info (fun f -> f "Opening tunnel for %S (%d)" user.Unix.pw_name uid);
    executor_of_cmd ~mgr:(Eio.Stdenv.process_mgr env) ~outer_sw ~sw (wait_for_init ~clock:env#clock)
      [ "sudo"; "--user"; user.Unix.pw_name; Sys.executable_name ]
  in
  let memo user =
    match ITbl.find_opt user_tunnels user with
    | Some x -> Promise.await x
    | None ->
        let await, resolve = Promise.create ~label:"User Tunnel" () in
        ITbl.replace user_tunnels user await;
        Fiber.fork ~sw (fun () -> Or_exn.run (fun () -> create user) |> Promise.resolve resolve);
        Promise.await await
  in
  let local = Executors.local ~env in
  let find user : Executor.t Or_exn.t =
    match find_user user with
    | Error msg -> Or_exn.Error msg
    | Ok user ->
        if user = current_user then Or_exn.Ok local
        else if current_user <> 0 then Or_exn.Error "Cannot use different users when not root"
        else memo user
  in
  find

let ssh ~(env : Eio_unix.Stdenv.base) ~outer_sw ~sw ({ sudo_pw; host; tunnel_path } : Remote.t) =
  let setup, cmd =
    let ssh = Sys.getenv_opt "SCRUTINY_SSH" |> Option.value ~default:"ssh" in
    match sudo_pw with
    | None -> (wait_for_init ~clock:env#clock, [ ssh; host; tunnel_path ])
    | Some password ->
        let prompt ~input ~output =
          (* TODO: Wait for prompt rather than sleeping. *)
          Eio.Time.sleep env#clock 0.5;
          Eio.Buf_write.string output password;
          Eio.Buf_write.char output '\n';
          wait_for_init ~clock:env#clock ~input ~output
        in
        (prompt, [ ssh; host; "sudo"; "-S"; "-p"; "sudo[scrutiny-infra-tunnel]: "; tunnel_path ])
  in
  executor_of_cmd ~mgr:(Eio.Stdenv.process_mgr env) ~outer_sw ~sw setup cmd

let run_tunnel ~env () : unit =
  (if Option.is_none (Sys.getenv_opt "XDG_RUNTIME_DIR") then
     let id = Unix.getuid () in
     if id <> 0 then Unix.putenv "XDG_RUNTIME_DIR" (Printf.sprintf "/run/user/%d" id));

  Switch.run @@ fun outer_sw ->
  Switch.run @@ fun sw ->
  let next_action = ref 0 in
  let pending_actions = ITbl.create 32 in
  let run_as_user = make_executor_factory ~env ~outer_sw ~sw () in

  let check_resource (AppliedResource.Boxed { resource; key; value; options; user }) : check_result
      =
    let result =
      match run_as_user user with
      | (Error _ | Exception _) as e -> e
      | Ok executor -> executor.apply ~user:`Current resource key value options
    in
    match result with
    | (Error _ | Exception _) as e -> e
    | Ok Executor.ECorrect -> Ok `Correct
    | Ok (Executor.ENeedsChange { diff; apply }) ->
        let action_id = !next_action in
        next_action := action_id + 1;
        ITbl.replace pending_actions action_id apply;
        Ok (`NeedsChange (action_id, diff))
  in

  let apply_resource id =
    match ITbl.find_opt pending_actions id with
    | None -> failwith "No such resource"
    | Some action -> action ()
  in

  let rpc =
    Scrutiny_rpc.create ~sw
      [
        Scrutiny_rpc.Method.handle Signatures.check_resource check_resource;
        Scrutiny_rpc.Method.handle Signatures.apply_resource apply_resource;
      ]
      (Eio.Buf_read.of_flow ~max_size:(1024 * 1024) env#stdin)
      (env#stdout :> Eio.Flow.sink)
  in

  (* Set up our forwarding log reporter. *)
  Scrutiny_rpc.log_forwarder rpc |> Logs.set_reporter;

  (* We send an "init" message which contains the version information. *)
  Eio.Flow.copy_string (magic_prefix ^ our_version ^ "\n") env#stdout;

  Scrutiny_rpc.await_closed rpc
