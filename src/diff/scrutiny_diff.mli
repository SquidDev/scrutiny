(** Represents the diff between two objects.

    This is a very simple implementation - it's designed to be a quick reference on the terminal
    rather than any "useful" term. *)

type t

(** Display this diff. If [full] is false (the default), only changed items are shown. *)
val pp : ?full:bool -> Format.formatter -> t -> unit

type change =
  [ `Same
  | `Remove
  | `Add
  ]

(** Determine if this diff has no changes. *)
val is_empty : t -> bool

(** An empty diff. *)
val empty : t

(** Make a diff from a list of strings.. *)
val of_lines : (change * string) list -> t

(** Make a diff from a single value. Either returns a single line of [`Same] or a pair of [`Remove]
    and [`Add]. *)
val of_line : old:string -> new_:string -> t

(** Make a diff from a two multi-line strings. *)
val of_diff : old:string -> new_:string -> t

(** Make a diff from a map of objects and different objects. *)
val structure : (string * t) list -> t

module Structure : sig
  type 'a row =
    { name : string;
      diff : 'a -> 'a -> t;
      basic : change -> 'a -> t
    }

  val row : name:string -> pp:('b -> string) -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a row

  val compare : 'a row list -> 'a option -> 'a option -> t

  val map : ('b -> 'a) -> 'a row -> 'b row
end
