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
