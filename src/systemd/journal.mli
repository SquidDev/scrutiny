type t

type flags = LocalOnly

type journal_change =
  | Nop
  | Append
  | Invalidate
  | Unknown of int

(** The level of log entries. *)
type level =
  | Alert
  | Crit
  | Error
  | Warning
  | Notice
  | Info
  | Debug

(** Open the current journal, defaulting to the local one. *)
val open_ : ?flags:flags list -> unit -> t

(** Close a journal. *)
val close : t -> unit

(** Wait for a new journal event. *)
val wait : ?timeout:Unsigned.uint64 -> t -> journal_change

(** Wait for a new journal event. *)
val wait_async : t -> journal_change Lwt.t

(** Read the next journal event, returning false when reaching the end of the DB *)
val next : t -> bool

(** Skip to the end of the journal. *)
val seek_tail : t -> unit

(** Get a field in the current journal entry as a raw pointer and length. *)
val get_data_raw_exn : t -> string -> (char, [ `C ]) Ctypes.pointer * int

(** Get a field in the current journal entry. *)
val get_data_exn : t -> string -> string

(** Get a field in the current journal entry as a raw pointer and length. *)
val get_data_raw : t -> string -> ((char, [ `C ]) Ctypes.pointer * int) option

(** Get a field in the current journal entry. *)
val get_data : t -> string -> string option

(** Get the level of the current journal entry. Defaults to {!Error} if no level is specified. *)
val get_level : t -> level

(** Utilities for writing to the journal. *)
module Write : sig
  (** Write a value to the syslog with a given tag ([SYSLOG_IDENTIFIER]), level ([PRIORITY]) and
      other fields .*)
  val write : ?tag:string -> ?level:level -> ?extra:(string * string) list -> string -> unit

  (** Write to the syslog with a given set of fields. *)
  val write_ : string list -> unit
end
