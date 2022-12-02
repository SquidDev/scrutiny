(** Support for annotating log messages with the currently running rule. *)
module Log_tag : sig
  (** The key currently being built. *)
  val tag : Core.Concrete_key.boxed Logs.Tag.def

  (** Wrap a log reporter, adding additional tags before dispatching to the original one. *)
  val wrap : Logs.reporter -> Logs.reporter
end

type run_result = {
  total : int;
  changed : int;
  failed : int;
}

type progress = {
  key_start : Core.Concrete_key.boxed -> unit;
  key_done : Core.Concrete_key.boxed -> unit;
}

(** Apply a batch of rules. *)
val apply :
  env:Eio.Stdenv.t ->
  ?progress:progress ->
  ?dry_run:bool ->
  Core.Rules.rules ->
  (run_result, string) result
