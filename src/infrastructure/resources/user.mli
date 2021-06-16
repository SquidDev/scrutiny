type t =
  [ `Current  (** The current user or group. *)
  | `Id of int  (** A user or group with a given id. *)
  | `Name of string  (** A user or group with a given name. *)
  ]

(** Convert a user identifier to a JSON object. *)
val yojson_of_t : t -> Yojson.Safe.t

(** Construct a user identifier from a JSON object. *)
val t_of_yojson : Yojson.Safe.t -> t

(** Convert a user identifier to a uid. *)
val uid_of : t -> (int, string) result Lwt.t

(** Convert a group identifier to a gid. *)
val gid_of : t -> (int, string) result Lwt.t
