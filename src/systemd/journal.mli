type t
type flags = LocalOnly  (** SD_JOURNAL_LOCAL_ONLY *)

type journal_change =
  | Nop
  | Append
  | Invalidate
  | Unknown of int

(** The level of log entries. *)
type level =
  | Emerg  (** LOG_EMERG *)
  | Alert  (** LOG_ALERT *)
  | Crit  (** LOG_CRIT *)
  | Error  (** LOG_ERROR *)
  | Warning  (** LOG_WARNING *)
  | Notice  (** LOG_NOTICE *)
  | Info  (** LOG_INFO *)
  | Debug  (** LOG_DEBUG *)

(** Open the current journal, defaulting to the local one. *)
val open_ : sw:Eio.Std.Switch.t -> ?flags:flags list -> unit -> t

(** Wait for a new journal event.

    {b Note:} if the journal is closed while we're waiting here, the promise may not complete. *)
val wait : t -> journal_change

(** Read the next journal event, returning false when reaching the end of the DB *)
val next : t -> bool

(** Skip to the end of the journal. *)
val seek_tail : t -> unit

(** Get a field in the current journal entry. *)
val get_data_exn : t -> string -> string

(** Get a field in the current journal entry. *)
val get_data : t -> string -> string option

(** Get the realtime timestamp for the current journal entry. *)
val get_realtime : t -> float

(** Get the level of the current journal entry. Defaults to {!Error} if no level is specified. *)
val get_level : t -> level

(** Utilities for writing to the journal. *)
module Write : sig
  (** Write a value to the syslog with a given tag ([SYSLOG_IDENTIFIER]), level ([PRIORITY]) and
      other fields .*)
  val write : ?tag:string -> ?level:level -> ?extra:(string * string) list -> string -> unit

  (** Write to the syslog with a given set of fields. *)
  val write_fields : string list -> unit

  (** Write to the syslog with a given set of fields. *)
  val write_fields_array : string array -> unit
end
