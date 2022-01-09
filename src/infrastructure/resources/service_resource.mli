open Scrutiny_infrastructure

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
