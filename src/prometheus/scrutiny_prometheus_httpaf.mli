(** http/af handler for Prometheus. *)

(** Render the given snapshot to a response. This assumes the request is to [/metrics]. *)
val handle : Prometheus.CollectorRegistry.snapshot -> Httpaf.Reqd.t -> Httpaf.Request.t -> unit

(** Calls [/handle] *)
val handle_default : Httpaf.Reqd.t -> Httpaf.Request.t -> unit
