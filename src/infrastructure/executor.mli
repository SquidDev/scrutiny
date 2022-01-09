(** Executors are responsible for actually running the action. *)

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
