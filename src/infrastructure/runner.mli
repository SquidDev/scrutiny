(** Apply a batch of rules. *)
val apply :
  ?progress:(int -> unit) -> ?executor:Executor.t -> ?dry_run:bool -> Core.Rules.rules -> unit Lwt.t
