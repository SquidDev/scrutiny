(** Executors are responsible for actually running the action. *)

module PartialKey : sig
  type t = PKey : ('key, 'value, 'options) Core.Resource.t * 'key -> t

  include Hashtbl.HashedType with type t := t

  (** A log tag denoting the current key a log message is associated with. *)
  val tag : t Logs.Tag.def

  (** Run a continuation with the given context in scope. *)
  val with_context : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

(** Wrap a log reporter, adding additional tags before dispatching to the original one. *)
val wrap_logger : Logs.reporter -> Logs.reporter

type change =
  | ECorrect
  | ENeedsChange of {
      diff : Scrutiny_diff.t;
      apply : unit -> unit Or_exn.t Lwt.t;
    }

type t = {
  apply :
    'key 'value 'options.
    user:Core.user ->
    ('key, 'value, 'options) Core.Resource.t ->
    'key ->
    'value ->
    'options ->
    change Or_exn.t Lwt.t;
}

(** An executor which applies actions to the local machine. *)
val local : t
