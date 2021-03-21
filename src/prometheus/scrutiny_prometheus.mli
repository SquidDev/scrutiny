(** Formatting for [v0.0.4] of Prometheus's export format. *)
module TextFormat_0_0_4 : sig
  val output : Format.formatter -> Prometheus.CollectorRegistry.snapshot -> unit
end
