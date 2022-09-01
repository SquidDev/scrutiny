module I = Scrutiny_infrastructure
open Scrutiny_infrastructure_resources_internal
module SMap = Map.Make (String)
module User = User

module File = struct
  open File_resource

  type nonrec file_state = file_state = {
    user : User.t;
    group : User.t;
    perms : int;
    contents : string;
    make_dirs : bool;
  }

  let file path (action : unit -> (file_state Lwt.t, unit) I.Action.t) =
    I.Rules.resource file_resource path action

  let file' path ?(user = `Current) ?group ?(perms = 0o644) ?(make_dirs = true) contents =
    file path @@ fun () ->
    let open I.Action in
    let+ contents = contents () in
    Lwt.return { user; group = Option.value ~default:user group; make_dirs; perms; contents }

  let env = { Jingoo.Jg_types.std_env with strict_mode = true; autoescape = false }

  let template path ?user ?group ?perms ?make_dirs ~template params =
    file' path ?user ?group ?perms ?make_dirs @@ fun () ->
    let open I.Action in
    let+ params = params () in
    let params = List.to_seq params |> SMap.of_seq in
    let models name =
      match SMap.find_opt name params with
      | Some x -> x
      | None -> raise Not_found
      (* TODO: invalid_arg (Printf.sprintf "Variable %S is not defined" name) *)
    in
    Jingoo.Jg_template2.from_file ~env ~models (Fpath.to_string template)
end

module Directory = struct
  open Directory_resource

  type nonrec dir_state = dir_state = {
    user : User.t;
    group : User.t;
    perms : Unix.file_perm;
  }

  let directory path (action : unit -> (dir_state Lwt.t, unit) I.Action.t) =
    I.Rules.resource dir_resource path action

  let directory' path ?(user = `Current) ?group ?(perms = 0o755)
      (action : unit -> (unit, unit) I.Action.t) =
    directory path @@ fun () ->
    let open I.Action in
    let+ () = action () in
    Lwt.return { user; group = Option.value ~default:user group; perms }
end

module Service = Service_resource
module Dns = Dns_resource
