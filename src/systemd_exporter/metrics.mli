(** Config controlling what metrics are gathered. *)
type t =
  { busses : OBus_bus.t list;
    cgroups : Cgroups.t
  }

(** Get the current units's state and update the metrics counter. *)
val collect : t -> unit Lwt.t

(** The default config options. *)
val default_options : unit -> t Lwt.t
