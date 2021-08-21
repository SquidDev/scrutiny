module Infra = Scrutiny_infrastructure

module DirState = struct
  type t =
    { user : User.t;
      group : User.t;
      perms : int
    }
  [@@deriving yojson]

  let digest x = Yojson.Safe.to_string (yojson_of_t x)
end

module Dir = struct
  let id = "dir"

  module Key = Infra.Path
  module EdgeOptions = Infra.Unit
  module Value = DirState

  let pp out x = Fmt.fmt "Directory %a" out Fpath.pp x

  let get_state path : (File_mod.t option, string) result Lwt.t =
    let path_s = Fpath.to_string path in
    match%lwt Lwt_unix.stat path_s with
    | { st_kind = S_DIR; st_uid; st_gid; st_perm; _ } ->
        Lwt.return_ok (Some { File_mod.user = st_uid; group = st_gid; perms = st_perm })
    | _ -> Lwt.return_error "Path exists, but is not a file"
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_ok None
    | exception Unix.Unix_error (code, _, _) ->
        Lwt.return_error
          (Format.sprintf "Failed to get current state (%s)" (Unix.error_message code))

  let state_to_partial { DirState.user; group; perms } =
    let%lwt user = User.uid_of user and group = User.gid_of group in
    match (user, group) with
    | Error e, _ | _, Error e -> Lwt.return_error e
    | Ok user, Ok group -> Lwt.return_ok { File_mod.user; group; perms }

  let apply path (target : DirState.t) () : (Infra.change, string) result Lwt.t =
    let%lwt current = get_state path and target = state_to_partial target in
    match (current, target) with
    | Error e, _ | _, Error e -> Lwt.return_error e
    | Ok (Some current), Ok target
      when current.user = target.user && current.group = target.group
           && current.perms = target.perms ->
        Lwt.return_ok Infra.Correct
    | Ok _, Ok target ->
        let diff = Scrutiny_diff.Structure.compare File_mod.rows None (Some target) in
        let apply () =
          match%lwt Lwt_unix.mkdir (Fpath.to_string path) target.perms with
          | () -> File_mod.apply ~current:None ~target path
          | exception Unix.Unix_error (code, _, _) ->
              Lwt.return_error
                (Format.sprintf "Failed to create directory (%s)" (Unix.error_message code))
        in
        Lwt.return_ok (Infra.NeedsChange { diff; apply })
end

let dir_module =
  Infra.Resource.make
    (module Dir : Infra.Resource
      with type Key.t = Fpath.t
       and type EdgeOptions.t = unit
       and type Value.t = DirState.t)

type dir_state = DirState.t =
  { user : User.t;
    group : User.t;
    perms : Unix.file_perm
  }

let directory path (action : unit -> (DirState.t Lwt.t, unit) Infra.Action.t) =
  Infra.Rules.resource dir_module path action

let directory' path ?(user = `Current) ?group ?(perms = 0o755)
    (action : unit -> (unit, unit) Infra.Action.t) =
  directory path @@ fun () ->
  let open Infra.Action in
  let+ () = action () in
  Lwt.return { user; group = Option.value ~default:user group; perms }
