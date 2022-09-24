(** Set up log levels.

    This behaves largely the same as {!Logs.set_level}, but with specific overrides for some
    libraries to ensure their logs aren't too noisy. *)
val setup_logs : ?extra_level:Logs.level -> Logs.level -> unit

val main : ([ `Local ], unit) Core.Rules.t -> unit
