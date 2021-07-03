(** Apply a batch of rules. *)
val apply :
  ?progress:(int -> unit) -> ?switch:Lwt_switch.t -> ?dry_run:bool -> Core.Rules.rules -> unit Lwt.t
