(** Runs commands over stdin/stdout. This allows us to run states on remote servers, etc... *)

(** Wait for a tunnel to be fully established. *)
val wait_for_init : Lwt_process.process -> (unit, string) result Lwt.t

(** Start a new process which runs a tunnel. *)
val run_tunnel : ?switch:Lwt_switch.t -> unit -> unit Lwt.t

(** Start a process and construct an executor from that process. *)
val executor_of_cmd :
  ?switch:Lwt_switch.t ->
  (Lwt_process.process -> (unit, string) result Lwt.t) ->
  string array ->
  (Executor.t, string) result Lwt.t

val ssh : ?switch:Lwt_switch.t -> Core.Remote.t -> (Executor.t, string) result Lwt.t
