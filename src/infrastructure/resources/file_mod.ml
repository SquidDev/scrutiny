(** Modify a node on the filesystem, enforcing ownership and permissions. *)

type t = {
  user : int;
  group : int;
  perms : int;
}

let fields =
  let open Scrutiny_diff.Structure in
  [
    field ~name:"user" ~pp:string_of_int (fun x -> x.user);
    field ~name:"group" ~pp:string_of_int (fun x -> x.group);
    field ~name:"perms" ~pp:(Format.sprintf "%03o") (fun x -> x.perms);
  ]

(** Read the current state of the path. *)
let stat ~expected_kind ~expected_kind_str path =
  match Eio_unix_async.stat (Fpath.to_string path) with
  | { st_kind; st_uid; st_gid; st_perm; _ } ->
      if st_kind = expected_kind then Ok (Some { user = st_uid; group = st_gid; perms = st_perm })
      else Error ("Path exists, but is not a " ^ expected_kind_str)
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
  | exception Unix.Unix_error (code, _, _) ->
      Error (Format.sprintf "Failed to get current state (%s)" (Unix.error_message code))

(** Apply a state to the current *)
let apply ~current ~target:{ user; group; perms } path =
  let ( let>> ) = Result.bind in
  let path = Fpath.to_string path in

  let>> () =
    let need_chown =
      Option.fold ~none:true
        ~some:(fun current -> current.user <> user || current.group <> group)
        current
    in
    match if need_chown then Eio_unix_async.chown path user group else () with
    | () -> Ok ()
    | exception Unix.Unix_error (code, _, _) ->
        Error (Format.sprintf "Failed to set user/owner (%s)" (Unix.error_message code))
  in

  let>> () =
    let need_chmod =
      Option.fold ~none:false ~some:(fun current -> current.perms <> perms) current
    in
    match if need_chmod then Eio_unix_async.chmod path perms else () with
    | () -> Ok ()
    | exception Unix.Unix_error (code, _, _) ->
        Error (Format.sprintf "Failed to set permissions (%s)" (Unix.error_message code))
  in
  Ok ()
