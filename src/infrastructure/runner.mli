type progress = {
  key_start : Core.boxed_key -> unit;
  key_done : Core.boxed_key -> unit;
}

(** Apply a batch of rules. *)
val apply :
  ?progress:progress -> ?switch:Lwt_switch.t -> ?dry_run:bool -> Core.Rules.rules -> unit Lwt.t
