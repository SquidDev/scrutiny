module Infra = Scrutiny_infrastructure
module SMap = Map.Make (String)

let hash_file contents =
  let module H = Digestif.SHA256 in
  let h = H.init () in
  let h = H.feed_string h contents in
  H.get h |> H.to_hex

(** The concrete "derived" file state. *)
module FileState = struct
  type t = {
    user : User.t;
    group : User.t;
    perms : int;
    contents : string;
    make_dirs : bool;
  }
  [@@deriving yojson]

  let yojson_of_t_hashed ({ user; group; perms; contents; make_dirs } : t) : Yojson.Safe.t =
    `Assoc
      [
        ("user", User.yojson_of_t user);
        ("group", User.yojson_of_t group);
        ("perms", `Int perms);
        ("contents", `String (hash_file contents));
        ("make_dirs", `Bool make_dirs);
      ]

  let digest x = Yojson.Safe.to_string (yojson_of_t_hashed x)
end

module File = struct
  let id = "file"

  module Key = Infra.Path
  module EdgeOptions = Infra.Unit
  module Value = FileState

  let pp out x = Fmt.fmt "File %a" out Fpath.pp x

  type partial_state = {
    file_mod : File_mod.t;
    contents : string;
  }

  let rows : partial_state Scrutiny_diff.Structure.row list =
    let open Scrutiny_diff.Structure in
    List.map (map (fun x -> x.file_mod)) File_mod.rows
    @ [
        {
          name = "contents";
          diff = (fun x y -> Scrutiny_diff.of_diff ~old:x.contents ~new_:y.contents);
          basic =
            (fun change x ->
              String.split_on_char '\n' x.contents
              |> List.map (fun l -> (change, l))
              |> Scrutiny_diff.of_lines);
        };
      ]

  let get_current_state path =
    match%lwt File_mod.stat ~expected_kind:S_REG ~expected_kind_str:"file" path with
    | Error e -> Lwt.return_error e
    | Ok None -> Lwt.return_ok None
    | Ok (Some file_mod) ->
        let%lwt contents = Lwt_io.with_file ~mode:Lwt_io.input (Fpath.to_string path) Lwt_io.read in
        Lwt.return_ok (Some { file_mod; contents })

  let state_to_partial { FileState.user; group; contents; perms; _ } =
    let%lwt user = User.uid_of user and group = User.gid_of group in
    match (user, group) with
    | Error e, _ | _, Error e -> Lwt.return_error e
    | Ok user, Ok group -> Lwt.return_ok { file_mod = { user; group; perms }; contents }

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

  let do_apply ~path (current : partial_state option) (target : partial_state) :
      (unit, string) result Lwt.t =
    let%lwt () =
      if is_dirty (fun x -> x.contents) current target.contents then (
        mkdirs (Fpath.parent path);
        Lwt_io.with_file ~perm:target.file_mod.perms ~mode:Lwt_io.output (Fpath.to_string path)
        @@ fun h -> Lwt_io.write h target.contents)
      else Lwt.return ()
    in
    File_mod.apply ~current:(Option.map (fun x -> x.file_mod) current) ~target:target.file_mod path

  let apply path (target : FileState.t) () : (Infra.change, string) result Lwt.t =
    let%lwt current = get_current_state path and target' = state_to_partial target in
    match (current, target') with
    | Error e, _ | _, Error e -> Lwt.return_error e
    | Ok (Some current), Ok target
      when current.file_mod = target.file_mod && current.contents = target.contents ->
        Lwt.return_ok Infra.Correct
    | Ok current, Ok target ->
        let change =
          Infra.NeedsChange
            {
              diff = Scrutiny_diff.Structure.compare rows current (Some target);
              apply = (fun () -> do_apply ~path current target);
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

type file_state = FileState.t = {
  user : User.t;
  group : User.t;
  perms : int;
  contents : string;
  make_dirs : bool;
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
