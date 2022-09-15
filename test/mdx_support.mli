(** Helper functions for MDX. *)
open Eio.Std

(** A polymorphic way to run functions which consume some scoped object. *)
type 'arg scoped = { run : 'a. ('arg -> 'a) -> 'a }

(** Produce a pair of flows which feed into each other. This is equivalent to {!Unix.pipe}, but
    without the dependency on Unix. *)
val pipe : sw:Switch.t -> string -> Eio.Flow.source * Eio.Flow.sink

(** Create a new {!pipe} and wrap them as buffered readers/writers. *)
val with_buffered_pipe : string -> (Eio.Buf_read.t * Eio.Flow.sink -> 'a) -> 'a

(** Create a switch, run a function, and then immediately kill the switch. *)
val with_sw : (Switch.t -> unit) -> unit

(** Run a function with its own separate logger. *)
val with_scoped_logger : (Logs.reporter ref scoped -> 'a) -> 'a

(** Create a new log reporter with the given name. *)
val named_reporter : string -> Logs.reporter
