open Lwt.Infix

type t =
  [ `Current
  | `Name of string
  | `Id of int
  ]

let yojson_of_t : t -> Yojson.Safe.t = function
  | `Current -> `Null
  | `Name user -> `String user
  | `Id uid -> `Int uid

let t_of_yojson : Yojson.Safe.t -> t = function
  | `Null -> `Current
  | `String x -> `Name x
  | `Int x -> `Id x
  | _ -> raise (Yojson.Json_error "Invalid user")

let find_user_id ~kind ~default ~lookup : t -> (int, string) result Lwt.t = function
  | `Current -> Lwt.return_ok (default ())
  | `Id id -> Lwt.return_ok id
  | `Name name ->
      Lwt.try_bind
        (fun () -> lookup name)
        Lwt.return_ok
        (function
          | Not_found -> Lwt.return_error (Printf.sprintf "Unknown %s %S" kind name)
          | e -> raise e)

let uid_of =
  find_user_id ~kind:"user" ~default:Unix.getuid ~lookup:(fun name ->
      Lwt_unix.getpwnam name >|= fun ent -> ent.Unix.pw_uid)

let gid_of =
  find_user_id ~kind:"group" ~default:Unix.getgid ~lookup:(fun name ->
      Lwt_unix.getgrnam name >|= fun ent -> ent.Unix.gr_gid)
