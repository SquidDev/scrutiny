(** Apply a batch of rules. *)
val apply : ?executor:Executor.t -> ?dry_run:bool -> unit Core.Rules.t -> unit Lwt.t
