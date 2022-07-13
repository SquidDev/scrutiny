(** Runs commands over stdin/stdout. This allows us to run states on remote servers, etc... *)

(** Start a new process which runs a tunnel. *)
val run_tunnel : ?switch:Lwt_switch.t -> unit -> unit Lwt.t

val ssh : ?switch:Lwt_switch.t -> Core.Remote.t -> (Executor.t, string) result Lwt.t
