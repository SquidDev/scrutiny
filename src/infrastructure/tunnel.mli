(** Runs commands over stdin/stdout. This allows us to run states on remote servers, etc... *)

(** Start a new process which runs a tunnel. *)
val run_tunnel : env:Eio_unix.Stdenv.base -> unit -> unit

val ssh :
  env:Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> Core.Remote.t -> (Core.Executor.t, string) result
