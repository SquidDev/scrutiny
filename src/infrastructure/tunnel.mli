(** Runs commands over stdin/stdout. This allows us to run states on remote servers, etc... *)

(** Start a new process which runs a tunnel. *)
val run_tunnel : env:Eio.Stdenv.t -> unit -> unit

val ssh : sw:Eio.Switch.t -> Core.Remote.t -> (Executor.t, string) result
