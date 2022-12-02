(** Utilities for running Unix functions asynchronously under Eio. *)

(** Run an operation on a separate thread.

    This uses a global thread for all operations (unlike Eio_unix which spawns a thread for each
    operation). As such, only short lived operations should run on this thread. *)
val run : name:string -> (unit -> 'a) -> 'a

(** {2 Unix wrappers}

    These simply wrap definitions in the {!Unix} module. *)

(** See {!Unix.stat} *)
val stat : string -> Unix.stats

(** See {!Unix.chown} *)
val chown : string -> int -> int -> unit

(** See {!Unix.chmod} *)
val chmod : string -> int -> unit

(** See {!Unix.getpwnam} *)
val getpwnam : string -> Unix.passwd_entry

(** See {!Unix.getgrnam} *)
val getgrnam : string -> Unix.group_entry

(** See {!Unix.getpwuid} *)
val getpwuid : int -> Unix.passwd_entry
