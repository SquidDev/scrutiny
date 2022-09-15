(** Utilities for spawning new processes under eio. *)

(** {1 Basic process manipulation} *)

(** A handle to a currently running process. *)
type t

(** Spawn a new process. *)
val spawn :
  sw:Eio.Switch.t ->
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr ->
  ?stderr:Unix.file_descr ->
  string ->
  string array ->
  t

(** Wait for a process to die. *)
val wait : t -> Unix.process_status

(** Get the current process status. *)
val status : t -> Unix.process_status option

(** Kill a process with the provided signal. *)
val kill : signal:int -> t -> unit

(** {1 Process input/output} *)

type channels = {
  stdin : Eio.Flow.sink;
  stdout : Eio.Flow.source;
  stderr : Eio.Flow.source;
}

(** Spawn a new process returning channels for the three *)
val spawn_full : sw:Eio.Switch.t -> string -> string array -> t * channels
