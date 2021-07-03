open Scrutiny_infrastructure

type dir_state =
  { user : User.t;
    group : User.t;
    perms : Unix.file_perm
  }

(** Ensure a directory exists. *)
val directory :
  Fpath.t -> (unit -> (dir_state Lwt.t, unit) Action.t) -> ('ctx, (unit, [ `Resource ]) key) Rules.t

(** A simpler version of {!directory}, where nothing varies. *)
val directory' :
  Fpath.t ->
  ?user:User.t ->
  ?group:User.t ->
  ?perms:Unix.file_perm ->
  (unit -> (unit, unit) Action.t) ->
  ('ctx, (unit, [ `Resource ]) key) Rules.t
