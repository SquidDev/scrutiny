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

let find_user_id ~kind ~default ~lookup : t -> (int, string) result = function
  | `Current -> Ok (default ())
  | `Id id -> Ok id
  | `Name name -> (
    match lookup name with
    | x -> Ok x
    | exception Not_found -> Error (Printf.sprintf "Unknown %s %S" kind name))

let uid_of =
  find_user_id ~kind:"user" ~default:Unix.getuid ~lookup:(fun name ->
      let ent = Eio_unix_async.getpwnam name in
      ent.Unix.pw_uid)

let gid_of =
  find_user_id ~kind:"group" ~default:Unix.getgid ~lookup:(fun name ->
      let ent = Eio_unix_async.getgrnam name in
      ent.Unix.gr_gid)
