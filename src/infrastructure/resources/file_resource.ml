module Infra = Scrutiny_infrastructure
module SMap = Map.Make (String)

let hash_file contents =
  let module H = Digestif.SHA256 in
  let h = H.init () in
  let h = H.feed_string h contents in
  H.get h |> H.to_hex

(** The concrete "derived" file state. *)
module FileState = struct
  type t =
    { user : User.t;
      group : User.t;
      perms : int;
      contents : string;
      make_dirs : bool
    }
  [@@deriving yojson]

  let yojson_of_t_hashed ({ user; group; perms; contents; make_dirs } : t) : Yojson.Safe.t =
    `Assoc
      [ ("user", User.yojson_of_t user);
        ("group", User.yojson_of_t group);
        ("perms", `Int perms);
        ("contents", `String (hash_file contents));
        ("make_dirs", `Bool make_dirs)
      ]

  let digest x = Yojson.Safe.to_string (yojson_of_t_hashed x)
end

module File = struct
  let id = "file"

  module Key = Infra.Path
  module EdgeOptions = Infra.Unit
  module Value = FileState

  let pp out x = Fmt.fmt "File %a" out Fpath.pp x

  type partial_state =
    { user : int;
      group : int;
      perms : int;
      contents : string
    }

  let get_current_state path =
    match%lwt Lwt_unix.stat (Fpath.to_string path) with
    | { st_kind = S_REG; st_uid; st_gid; st_perm; _ } ->
        let%lwt contents = Lwt_io.with_file ~mode:Lwt_io.input (Fpath.to_string path) Lwt_io.read in
        Lwt.return_ok (Some { user = st_uid; group = st_gid; perms = st_perm; contents })
    | _ -> Lwt.return_error "Path exists, but is not a file"
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_ok None

  let state_to_partial { FileState.user; group; contents; perms; _ } =
    let%lwt user = User.uid_of user and group = User.gid_of group in
    match (user, group) with
    | Error e, _ | _, Error e -> Lwt.return_error e
    | Ok user, Ok group -> Lwt.return_ok { user; group; contents; perms }

  let is_dirty f (current : partial_state option) target =
    match current with
    | None -> true
    | Some x -> f x <> target

  let mkdirs =
    let rec go xs path =
      let path_s = Fpath.(rem_empty_seg path |> to_string) in
      if Sys.file_exists path_s then List.iter (fun p -> Unix.mkdir p 0o755) xs
      else if Fpath.is_root path then failwith "Root directory doesn't exist!"
      else go (path_s :: xs) Fpath.(parent path |> normalize)
    in
    go []

  let do_apply ~path (current : partial_state option) (target : FileState.t) :
      (unit, string) result Lwt.t =
    let file_path = Fpath.to_string path in
    let%lwt () =
      if is_dirty (fun x -> x.contents) current target.contents then (
        mkdirs (Fpath.parent path);
        (* TODO: Set owner and perms. *)
        Lwt_io.with_file ~perm:target.perms ~mode:Lwt_io.output file_path @@ fun h ->
        Lwt_io.write h target.contents)
      else Lwt.return ()
    in
    Lwt.return_ok ()

  let apply path (target : FileState.t) () : (Infra.change, string) result Lwt.t =
    let%lwt current = get_current_state path and target' = state_to_partial target in
    match (current, target') with
    | Error e, _ | _, Error e -> Lwt.return_error e
    | Ok (Some current), Ok target
      when current.user = target.user && current.group = target.group
           && current.perms = target.perms
           && current.contents = target.contents ->
        Lwt.return_ok Infra.Correct
    | Ok None, Ok target' ->
        let open Scrutiny_diff in
        let change =
          Infra.NeedsChange
            { diff =
                structure
                  [ ("user", of_lines [ (`Add, string_of_int target'.user) ]);
                    ("group", of_lines [ (`Add, string_of_int target'.group) ]);
                    ("perms", of_lines [ (`Add, Printf.sprintf "%3o" target'.perms) ]);
                    ( "contents",
                      String.split_on_char '\n' target'.contents
                      |> List.map (fun x -> (`Add, x))
                      |> of_lines )
                  ];
              apply = (fun () -> do_apply ~path None target)
            }
        in
        Lwt.return_ok change
    | Ok (Some current), Ok target' ->
        let open Scrutiny_diff in
        let basic f current target = of_line ~old:(f current) ~new_:(f target) in
        let change =
          Infra.NeedsChange
            { diff =
                structure
                  [ ("user", basic string_of_int current.user target'.user);
                    ("group", basic string_of_int current.group target'.group);
                    ("perms", basic (Printf.sprintf "%3o") current.perms target'.perms);
                    ("contents", of_diff ~old:current.contents ~new_:target'.contents)
                  ];
              apply = (fun () -> do_apply ~path (Some current) target)
            }
        in
        Lwt.return_ok change
end

let file_module =
  Infra.Resource.make
    (module File : Infra.Resource
      with type Key.t = Fpath.t
       and type EdgeOptions.t = unit
       and type Value.t = FileState.t)

type file_state = FileState.t =
  { user : User.t;
    group : User.t;
    perms : int;
    contents : string;
    make_dirs : bool
  }

let file path (action : unit -> (FileState.t Lwt.t, unit) Infra.Action.t) =
  Infra.Rules.resource file_module path action

let file' path ?(user = `Current) ?group ?(perms = 0o644) ?(make_dirs = true) contents =
  file path @@ fun () ->
  let open Infra.Action in
  let+ contents = contents () in
  Lwt.return
    { FileState.user; group = Option.value ~default:user group; make_dirs; perms; contents }

let env = { Jingoo.Jg_types.std_env with strict_mode = true; autoescape = false }

let template path ?user ?group ?perms ?make_dirs ~template params =
  file' path ?user ?group ?perms ?make_dirs @@ fun () ->
  let open Infra.Action in
  let+ params = params () in
  let params = List.to_seq params |> SMap.of_seq in
  let models name =
    match SMap.find_opt name params with
    | Some x -> x
    | None -> raise Not_found
    (* TODO: invalid_arg (Printf.sprintf "Variable %S is not defined" name) *)
  in
  Jingoo.Jg_template2.from_file ~env ~models (Fpath.to_string template)
