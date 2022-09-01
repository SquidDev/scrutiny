open Scrutiny_infrastructure

module User : sig
  type t =
    [ `Current  (** The current user or group. *)
    | `Id of int  (** A user or group with a given id. *)
    | `Name of string  (** A user or group with a given name. *)
    ]
end

module File : sig
  type file_state = {
    user : User.t;
    group : User.t;
    perms : Unix.file_perm;
    contents : string;
    make_dirs : bool;
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
end

module Directory : sig
  type dir_state = {
    user : User.t;
    group : User.t;
    perms : Unix.file_perm;
  }

  (** Ensure a directory exists. *)
  val directory :
    Fpath.t ->
    (unit -> (dir_state Lwt.t, unit) Action.t) ->
    ('ctx, (unit, [ `Resource ]) key) Rules.t

  (** A simpler version of {!directory}, where nothing varies. *)
  val directory' :
    Fpath.t ->
    ?user:User.t ->
    ?group:User.t ->
    ?perms:Unix.file_perm ->
    (unit -> (unit, unit) Action.t) ->
    ('ctx, (unit, [ `Resource ]) key) Rules.t
end

module Service : sig
  type service_state = {
    enabled : bool;  (** This service should be enabled.*)
    running : bool;  (** This service should be running. *)
    monitor : int;  (** Monitor this service for n seconds. *)
  }

  val service_state : ?enabled:bool -> ?running:bool -> ?monitor:int -> unit -> service_state

  (** Ensure a service exists and is in a given state. *)
  val service :
    name:string ->
    scope:[ `System | `User ] ->
    (unit -> (service_state, [ `None | `Restart | `Reload ]) Action.t) ->
    ('ctx, (unit, [ `Resource ]) key) Rules.t
end

module Dns = Dns_resource
