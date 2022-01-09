(** {1 The core types for illuaminate} *)

(** A basic value which can be stored in a database. *)
module type BasicValue = sig
  type t

  (** A unique mapping to a string.

      Unlike {!to_yojson}, one does not need to be able to reconstruct the value, so it is
      appropriate to use hash functions if needed. *)
  val digest : t -> string

  (** Convert this value to JSON. This is used when sending the term over the wire. *)
  val yojson_of_t : t -> Yojson.Safe.t

  (** Read this value from JSON. This is used when receiving the term over the wire *)
  val t_of_yojson : Yojson.Safe.t -> t
end

(** A remote server that commands can be run on. *)
module Remote : sig
  type t

  val make : ?sudo_pw:string -> ?tunnel_path:string -> string -> t
end

(** A change which will be applied to a resource. *)
type change =
  | Correct  (** This resource is in the correct state and no changes need be made.*)
  | NeedsChange of {
      diff : Scrutiny_diff.t;
      apply : unit -> (unit, string) result Lwt.t;
    }
      (** This resource is not in the correct state. Contains a diff from the current to target
          state, and a function which will apply those changes. *)

(** Options which can be applied to an edge.

    Each incoming edge contains a set of options (defaulting to {!EdgeOptions.default}). If this
    edge has changed, its options are included in the resulting options passed to {!apply}.

    These options should not affect the resulting state of a resource (that's what {!Value.t} is
    for), but instead change how the resource is applied. For instance, services may wish to restart
    or reload if a configuration file is changed. *)
module type EdgeOptions = sig
  type t

  (** The default set of edge options. This is used when no edges have changed. *)
  val default : t

  (** Merge options from two edges which have changed. This should obey the monoid laws
      (assocativity, identity with {!default}). *)
  val union : t -> t -> t

  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

module type Resource = sig
  val id : string

  (** The key for this term. *)
  module Key : sig
    include BasicValue
    include Hashtbl.HashedType with type t := t
  end

  (** Options which can be applied to an edge. See the main documentation for details. *)
  module EdgeOptions : EdgeOptions

  (** The value this resource should store. *)
  module Value : BasicValue

  (** Pretty-print this key, including some description (e.g. "File abc.txt"). *)
  val pp : Key.t Fmt.t

  (** Determine if this resource is in the correct state. If not, returns the changes needed and a
      function to make those changes.

      The [apply] function itself should have no side effects, as this is used when performing
      dry-runs and other sanity checks. *)
  val apply : Key.t -> Value.t -> EdgeOptions.t -> (change, string) result Lwt.t
end

module Resource : sig
  type ('key, 'value, 'options) t

  val make :
    (module Resource
       with type Key.t = 'key
        and type Value.t = 'value
        and type EdgeOptions.t = 'options) ->
    ('key, 'value, 'options) t
end

type ('result, 'kind) key

(** An action is an applicative which consumes a series of dependencies and produces a "value" for
    this term. *)
module Action : sig
  type ('value, 'options) t

  val ( let+ ) : ('a, 'options) t -> ('a -> 'b) -> ('b, 'options) t
  val ( and+ ) : ('a, 'options) t -> ('b, 'options) t -> ('a * 'b, 'options) t

  (** State that this action (and thus resource) is required for this one to run. This adds an edge
      from one resource to another, with some optional {!Resource.EdgeOptions.t}. *)
  val need : ?options:'options -> ('result, 'kind) key -> ('result, 'options) t

  (** Return a value. *)
  val value : 'a -> ('a, 'options) t
end

(** A monad of rules. This declares a set of resources to create and dependencies between them. *)
module Rules : sig
  type ('ctx, 'res) t

  val ( let* ) : ('ctx, 'a) t -> ('a -> ('ctx, 'b) t) -> ('ctx, 'b) t
  val pure : 'a -> ('ctx, 'a) t

  (** Register a resource in this rule set. *)
  val resource :
    ('key, 'value, 'options) Resource.t ->
    'key ->
    (unit -> ('value Lwt.t, 'options) Action.t) ->
    ('ctx, (unit, [ `Resource ]) key) t

  (** Run a set of rules as a particular user. *)
  val with_user : string -> (unit -> ([ `User ], 'a) t) -> ('ctx, 'a) t

  (** Run a set of rules as a particular user. *)
  val with_user_uid : int -> (unit -> ([ `User ], 'a) t) -> ('ctx, 'a) t

  (** Run a command on a particular remote. *)
  val with_remote : Remote.t -> (unit -> ([ `Remote ], 'a) t) -> ([ `Local ], 'a) t
end

(** {2 Builtin {!BasicValue}s for some types} *)

module Unit : sig
  include BasicValue with type t = unit
  include Hashtbl.HashedType with type t := t
  include EdgeOptions with type t := t
end

module String : sig
  include BasicValue with type t = string
  include Hashtbl.HashedType with type t := t
end

module Path : sig
  include BasicValue with type t = Fpath.t
  include Hashtbl.HashedType with type t := t
end

(** {3 Applying states} *)

(** Apply a collection of rules. *)
val apply : ?dry_run:bool -> ([ `Local ], unit) Rules.t -> unit Lwt.t

(** Parse command line arguments and apply a set of rules. *)
val main : ([ `Local ], unit) Rules.t -> unit

(* Start an instance listening for instructions from stdin and printing to stdout. *)
val run_tunnel : unit -> unit
