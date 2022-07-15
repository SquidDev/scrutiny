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

(** Make a diff from a list of values. *)
val of_lines : (change * string) list -> t

(** Make a diff from a single value. Either returns a single line of [`Same] or a pair of [`Remove]
    and [`Add]. *)
val of_line : old:string -> new_:string -> t

(** Make a diff from a two multi-line strings. *)
val of_diff : old:string -> new_:string -> t

(** Make a diff from a list of field names and their corresponding diffs.

    The {!Structure} module provides utilities for computing diffs. *)
val structure : (string * t) list -> t

module Structure : sig
  (** A field within a structure of type ['a]. *)
  type 'a field = {
    name : string;  (** The name of this field. *)
    diff : 'a -> 'a -> t;  (** Compute a diff between two objects. *)
    basic : change -> 'a -> t;
        (** Convert this object into a diff of just the given change. This is used when a field is
            only present on one side. *)
  }

  (** [field ~name ~pp proj] defines a field [name] of a primitive (i.e. non-structured) type such
      as a string or number.

      [proj] defines a projection function from our structure of type ['a] to our field of type
      ['b]. *)
  val field : name:string -> pp:('b -> string) -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a field

  (** Compute the diff of two structures when given a list of fields. *)
  val diff : 'a field list -> 'a option -> 'a option -> t

  (** [map proj field] applies a projection function to a field, producing a new field which diffs
      [proj x] and [proj y] when applied to [x] and [y]. *)
  val map : ('b -> 'a) -> 'a field -> 'b field
end
