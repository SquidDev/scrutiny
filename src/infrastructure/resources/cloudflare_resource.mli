open Scrutiny_infrastructure

(** A basic cloudflare zone. *)
val zone :
  string ->
  auth:Scrutiny_cloudflare.auth ->
  (unit -> (Scrutiny_cloudflare.DnsRecord.Spec.t list, unit) Action.t) ->
  ([ `Local ], (unit, [ `Resource ]) key) Rules.t
