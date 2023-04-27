module Infra = Scrutiny_infrastructure

module DirState = struct
  type t = {
    user : User.t;
    group : User.t;
    perms : int;
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

  let get_state path : (File_mod.t option, string) result =
    let path_s = Fpath.to_string path in
    match Eio_unix_async.stat path_s with
    | { st_kind = S_DIR; st_uid; st_gid; st_perm; _ } ->
        Ok (Some { File_mod.user = st_uid; group = st_gid; perms = st_perm })
    | _ -> Error "Path exists, but is not a file"
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
    | exception Unix.Unix_error (code, _, _) ->
        Error (Format.sprintf "Failed to get current state (%s)" (Unix.error_message code))

  let state_to_partial { DirState.user; group; perms } =
    let user = User.uid_of user and group = User.gid_of group in
    match (user, group) with
    | Error e, _ | _, Error e -> Error e
    | Ok user, Ok group -> Ok { File_mod.user; group; perms }

  let apply ~env path (target : DirState.t) () : (Infra.change, string) result =
    let path' = Path_support.of_fpath ~env path in

    let current = get_state path and target = state_to_partial target in
    match (current, target) with
    | Error e, _ | _, Error e -> Error e
    | Ok (Some current), Ok target
      when current.user = target.user && current.group = target.group
           && current.perms = target.perms -> Ok Infra.Correct
    | Ok _, Ok target ->
        let diff = Scrutiny_diff.Structure.diff File_mod.fields None (Some target) in
        let apply () =
          match Eio.Path.mkdir ~perm:target.perms path' with
          (* TODO: mkdirs instead? What's the correct behaviour with perms? *)
          | () -> File_mod.apply ~current:None ~target path
          | exception (Eio.Io _ as exn) ->
              Error (Format.asprintf "Failed to create directory (%a)" Eio.Exn.pp exn)
        in
        Ok (Infra.NeedsChange { diff; apply })
end

let dir_resource =
  Infra.Resource.make
    (module Dir : Infra.Resource
      with type Key.t = Fpath.t
       and type EdgeOptions.t = unit
       and type Value.t = DirState.t)

type dir_state = DirState.t = {
  user : User.t;
  group : User.t;
  perms : Unix.file_perm;
}
