(** Config controlling what metrics are gathered. *)
type t = {
  busses : Scrutiny_systemd.Manager.connection list;
  cgroups : Cgroups.t;
}

(** Get the current units's state and update the metrics counter. *)
val collect : fs:Eio.Fs.dir Eio.Path.t -> t -> unit
