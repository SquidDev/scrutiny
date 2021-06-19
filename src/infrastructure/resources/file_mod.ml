type t =
  { user : int;
    group : int;
    perms : int
  }

let rows =
  let open Scrutiny_diff.Structure in
  [ row ~name:"user" ~pp:string_of_int (fun x -> x.user);
    row ~name:"group" ~pp:string_of_int (fun x -> x.group);
    row ~name:"perms" ~pp:(Format.sprintf "%03o") (fun x -> x.perms)
  ]

let stat ~expected_kind ~expected_kind_str path =
  match%lwt Lwt_unix.stat (Fpath.to_string path) with
  | { st_kind; st_uid; st_gid; st_perm; _ } ->
      if st_kind = expected_kind then
        Lwt.return_ok (Some { user = st_uid; group = st_gid; perms = st_perm })
      else Lwt.return_error ("Path exists, but is not a " ^ expected_kind_str)
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_ok None
  | exception Unix.Unix_error (code, _, _) ->
      Lwt.return_error (Format.sprintf "Failed to get current state (%s)" (Unix.error_message code))

let apply ~current ~target:{ user; group; perms } path =
  let open More_lets in
  let path = Fpath.to_string path in

  let>> () =
    let need_chown =
      Option.fold ~none:true
        ~some:(fun current -> current.user <> user || current.group <> group)
        current
    in
    match%lwt if need_chown then Lwt_unix.chown path user group else Lwt.return_unit with
    | () -> Lwt.return_ok ()
    | exception Unix.Unix_error (code, _, _) ->
        Lwt.return_error (Format.sprintf "Failed to set user/owner (%s)" (Unix.error_message code))
  in

  let>> () =
    let need_chmod =
      Option.fold ~none:false ~some:(fun current -> current.perms <> perms) current
    in
    match%lwt if need_chmod then Lwt_unix.chmod path perms else Lwt.return_unit with
    | () -> Lwt.return_ok ()
    | exception Unix.Unix_error (code, _, _) ->
        Lwt.return_error (Format.sprintf "Failed to set permissions (%s)" (Unix.error_message code))
  in
  Lwt.return_ok ()
