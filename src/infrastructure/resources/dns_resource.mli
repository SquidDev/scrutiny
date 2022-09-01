open Scrutiny_infrastructure

(** A basic DNS zone. *)
val zone :
  string ->
  source:Scrutiny_dns.source ->
  (unit -> (Scrutiny_dns.DnsRecord.Spec.t list, unit) Action.t) ->
  ([ `Local ], (unit, [ `Resource ]) key) Rules.t
