open Scrutiny_infrastructure

type file_state =
  { user : User.t;
    group : User.t;
    perms : Unix.file_perm;
    contents : string;
    make_dirs : bool
  }

(** Ensure a file exists with specific content. *)
val file :
  Fpath.t ->
  (unit -> (file_state Lwt.t, unit) Action.t) ->
  ('ctx, (unit, [ `Resource ]) key) Rules.t

(** A simpler version of {!file}, where only the contents varies. *)
val file' :
  Fpath.t ->
  ?user:User.t ->
  ?group:User.t ->
  ?perms:Unix.file_perm ->
  ?make_dirs:bool ->
  (unit -> (string, unit) Action.t) ->
  ('ctx, (unit, [ `Resource ]) key) Rules.t

(** Like {!file'}, but where the content is derived from a template. *)
val template :
  Fpath.t ->
  ?user:User.t ->
  ?group:User.t ->
  ?perms:Unix.file_perm ->
  ?make_dirs:bool ->
  template:Fpath.t ->
  (unit -> ((string * Jingoo.Jg_types.tvalue) list, unit) Action.t) ->
  ('ctx, (unit, [ `Resource ]) key) Rules.t
