(** An executor which applies actions to the local machine. *)
val local : env:Eio.Stdenv.t -> Core.Executor.t
