(** Interact with the systemd daemon over D-Bus. See
    https://www.freedesktop.org/software/systemd/man/org.freedesktop.systemd1.html documentation on
    the underlying D-Bus interface.*)

type t

module Unit : sig
  (** A unit managed by systemd, corresponding to the [org.freedesktop.systemd1.Unit] interface. *)
  type t

  (** Get the identifier for this unit. *)
  val id : t -> string

  (** Attempt to start this unit. *)
  val start : t -> (unit, string) result

  (** Attempt to stop this unit. *)
  val stop : t -> (unit, string) result

  (** Attempt to reload this unit. *)
  val reload : t -> (unit, string) result

  (** Attempt to restart this unit. *)
  val restart : t -> (unit, string) result

  (** Attempt to reload this unit. *)
  val enable : t -> unit

  (** Attempt to restart this unit. *)
  val disable : t -> unit

  (** Get whether the current configuration file has been loaded. *)
  val get_load_state : t -> string

  (** Get whether the unit is active or not.

      Will be one of ["active"], ["reloading"], ["inactive"], ["failed"], ["activating"],
      ["deactivating"]. *)
  val get_active_state : t -> string

  (** Get whether the unit is enabled or not.

      Will be one of ["enabled"], ["enabled-runtime"], ["linked"], ["linked-runtime"], ["masked"],
      ["masked-runtime"], ["static"], ["disabled"] or ["invalid"]. *)
  val get_unit_file_state : t -> string
end

module Service : sig
  (** A "service" unit, corresponding to the [org.freedesktop.systemd1.Service] interface. *)
  type t

  (** Get the corresponding unit of this service. *)
  val unit : t -> Unit.t

  (** Create a service from a unit. *)
  val of_unit : Unit.t -> t

  (** Find the control group of this service. *)
  val get_control_group : t -> string
end

(** The state of a unit in systemd. *)
type unit_state = private {
  id : string;
  description : string;
  load_state : string;
  active_state : string;
  sub_state : string;
  following : string;
  unit : Unit.t;
}

type connection =
  [ `User
  | `System
  | `Other_user of string
  | `Bus of string
  ]

(** Construct a systemd manager from a dbus connection.

    Note that this will keep the bus alive indefinitely. One should use
    {!Scrutiny_eio.run_background_switch} (or some other cancellation method) to ensure the switch
    is turned off when your main function has exited. *)
val of_bus : sw:Eio.Std.Switch.t -> connection -> t

(** List all available units. *)
val list_units : t -> unit_state list

(** Attempt to load a single unit. *)
val load_unit : t -> string -> Unit.t

(** Reload the systemd daemon. *)
val daemon_reload : t -> unit
