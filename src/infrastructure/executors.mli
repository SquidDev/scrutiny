(** An executor which applies actions to the local machine. *)
val local : env:Eio_unix.Stdenv.base -> Core.Executor.t
