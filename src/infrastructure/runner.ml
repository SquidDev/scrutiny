open Core
open Eio.Std
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

module Key_set =
  Het_map.Make
    (Concrete_key)
    (struct
      type 'a t = unit
    end)

module State = struct
  type 'a t =
    | Pending : (bool * 'a, unit) result Promise.t -> ('a * 'k) t
    | Finished : (bool * 'a, unit) result -> ('a * 'k) t
end

module State_map = Het_map.Make (Concrete_key) (State)

module Log_tag = struct
  let tag = Logs.Tag.def "BoxedKey" (fun out (Concrete_key.BKey key) -> Concrete_key.pp out key)

  (** An {!Eio.Fiber.key} marking which key we're currently running. *)
  let context : Concrete_key.boxed Eio.Fiber.key = Eio.Fiber.create_key ()

  let with_context key fn = Eio.Fiber.with_binding context key fn

  let wrap { Logs.report } =
    let owner_thread = Thread.self () |> Thread.id in
    let report src level ~over k msgf =
      (* Only call Fiber.get on the Eio thread. *)
      let key = if Thread.id (Thread.self ()) = owner_thread then Fiber.get context else None in
      report src level ~over k @@ fun f ->
      msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
      let tags =
        if Logs.Tag.mem tag tags then tags
        else
          match key with
          | Some key -> Logs.Tag.add tag key tags
          | None -> tags
      in
      f ?header ~tags fmt
    in
    { Logs.report }
end

type progress = {
  key_start : Core.Concrete_key.boxed -> unit;
  key_done : Core.Concrete_key.boxed -> unit;
}

let default_progress = { key_start = (fun _ -> ()); key_done = (fun _ -> ()) }

type t = {
  builders : Builder_map.t;  (** Keys which have an explicit build rule defined. *)
  keys : State_map.t;  (** Keys which are currently being built or have a result. *)
  sw : Switch.t;  (** The switch where keys are run. *)
  env : Eio.Stdenv.t;  (** The environment where keys are run. *)
  progress : progress;  (** Progress callbacks. *)
  dry_run : bool;
}

(** Evaluate an action, assuming all dependencies have finished building.

    This returns the resulting value, the union of all changed edges, and whether any incoming edge
    changed. *)
let eval_action (type options) ~options:(module E : Core.EdgeOptions with type t = options)
    ~(store : t) x =
  let rec go : type ret. (ret, options) action -> (ret * options * bool, unit) result = function
    | Pure v -> Ok (v, E.default, false)
    | Need (options, key) -> (
      match State_map.get store.keys key with
      | None -> failwith "Impossible: Key is not defined"
      | Some (Pending _) -> failwith "Impossible: Key has not finished building"
      | Some (Finished (Error ())) -> Error ()
      | Some (Finished (Ok (changed, ret))) ->
          Ok (ret, (if changed then options else E.default), changed))
    | Bind (x, f) -> go x |> Result.map (fun (res, options, changed) -> (f res, options, changed))
    | Pair (l, r) -> (
      match (go l, go r) with
      | Ok (l, l_options, l_changed), Ok (r, r_options, r_changed) ->
          Ok ((l, r), E.union l_options r_options, l_changed || r_changed)
      | Error (), _ | _, Error () -> Error ())
  in
  match go x with
  | Error () -> Error ()
  | Ok (value, options, changed) -> Ok (value, options, changed)

(** "Build" (i.e apply) a resource, assuming all dependencies have been built. *)
let build_resource (type key value options) ~store
    ({ resource; key; user; machine } : (key, value, options) Concrete_resource.t) { term; _ } =
  let module R = (val resource) in
  let value = Pair (term, Need (R.EdgeOptions.default, Machine machine)) in
  (* Attempt to evaluate the rule. This is 90% error handling and 10% actual code. Sorry! *)
  match eval_action ~options:(module R.EdgeOptions) ~store value with
  | Error () -> Error ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Log.err (fun f -> f "Exception getting value for %a: %a" R.pp key Fmt.exn_backtrace (e, bt));
      Error ()
  | Ok ((value, executor), options, _changed) -> (
      Log.debug (fun f -> f "Applying: %a" R.pp key);
      let ( let>> ) = Result.bind in
      let ( <?> ) action (err, exn) = Or_exn.log (module Log) action (err, exn) in
      let>> result =
        executor.apply ~user resource key value options
        <?> ("Failed to compute changes", "Exception computing changes")
      in
      match result with
      | ECorrect ->
          Log.debug (fun f -> f "No changes needed");
          Ok (false, ())
      | ENeedsChange { diff; apply } ->
          Log.info (fun f -> f "Applying %a:@\n%a" R.pp key (Scrutiny_diff.pp ~full:false) diff);
          if store.dry_run then Ok (true, ())
          else
            let>> () = apply () <?> ("Failed to apply changes", "Exception applying changes") in
            Ok (true, ()))

(** Build a variable, assuming all dependencies have been built. *)
let build_var ~store var value =
  match eval_action ~options:(module Types.Unit) ~store value.term with
  | Error () -> Error ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Log.err (fun f ->
          f "Exception getting value for %a: %a" Concrete_var.pp var Fmt.exn_backtrace (e, bt));
      Error ()
  | Ok (value, _options, changed) -> Ok (changed, value)

let build_machine ~store:{ env; sw; _ } (machine : Machine.t) =
  match machine with
  | Local -> Ok (false, Executors.local ~env)
  | Remote remote -> (
    match Tunnel.ssh ~env ~sw remote with
    | Ok exec -> Ok (false, exec)
    | Error e ->
        Log.err (fun f -> f "Failed to open tunnel to %s: %s" remote.host e);
        Error ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Log.err (fun f -> f "Failed to open tunnel to %s: %a" remote.host Fmt.exn_backtrace (e, bt));
        Error ())

(** Build a rule, assuming all dependencies have been built. *)
let build_impl (type r opts) ~store (key : (r * opts) Concrete_key.t)
    (builder : (r * opts) Key_builder.t) : (bool * r, unit) result =
  match (key, builder) with
  | Resource key, Resource builder -> build_resource ~store key builder
  | Var key, Var builder -> build_var ~store key builder
  | Machine key, Machine -> build_machine ~store key

let all_dependencies (type t) (key : t Concrete_key.t) (builder : t Key_builder.t) =
  let dependencies = Key_builder.dependencies builder in
  match key with
  (* We add an extra dependenciy on the machine, as that's also injected in eval_resource. *)
  | Resource r -> Concrete_key.BKey (Machine r.machine) :: dependencies
  | Var _ | Machine _ -> dependencies

(** Wait for all rules to be built, assuming they have already been started. *)
let rec await_rules ~store = function
  | [] -> Ok ()
  | Concrete_key.BKey x :: xs -> (
      let continue = function
        | Ok _ -> await_rules ~store xs
        | Error _ -> Error ()
      in
      match State_map.get_exn store.keys x with
      | Finished f -> continue f
      | Pending x -> Promise.await x |> continue)

(** Enqueue a rule to be built. *)
let rec start_rule ~store (Concrete_key.BKey key) =
  match State_map.get store.keys key with
  | Some _ -> ()
  | None -> (
      let builder =
        match (Builder_map.get store.builders key, key) with
        | Some builder, _ -> Some builder
        | None, Var _ -> Some (Var { edges = []; term = Pure [] })
        | None, Machine _ -> Some Machine
        | None, Resource _ -> None
      in
      match builder with
      | Some builder ->
          let promise, resolve =
            Promise.create ~label:(Format.asprintf "Build %a" Concrete_key.pp key) ()
          in
          State_map.set store.keys key (Pending promise);
          Fiber.fork ~sw:store.sw (fun () -> build_rule ~store key builder resolve)
      | None ->
          Log.err (fun f -> f "%a is not defined" Concrete_key.pp key);
          State_map.set store.keys key (Finished (Error ())))

and build_rule :
      'r 'opts.
      store:t ->
      ('r * 'opts) Concrete_key.t ->
      ('r * 'opts) Key_builder.t ->
      (bool * 'r, unit) result Promise.u ->
      unit =
 fun ~store key builder resolve ->
  let dependencies = all_dependencies key builder in
  List.iter (start_rule ~store) dependencies;
  let result : (bool * 'r, unit) result =
    match await_rules ~store dependencies with
    | Ok () ->
        store.progress.key_start (BKey key);
        Log_tag.with_context (BKey key) @@ fun () -> build_impl ~store key builder
    | Error () -> Error ()
  in

  State_map.set store.keys key (Finished result);
  Promise.resolve resolve result;
  store.progress.key_done (BKey key)

let check_graph rules =
  let visited = Key_set.create (Builder_map.length rules) in
  let rec go stack (BKey key as bkey : Concrete_key.boxed) =
    let rule = Builder_map.get rules key in
    if Option.is_none rule then Error "Missing node in rule list"
    else if
      List.exists
        (fun (Concrete_key.BKey other) -> Concrete_key.equal key other |> Het_map.Eq.to_bool)
        stack
    then
      Format.asprintf "Loop in dependency graph: %a -> %a"
        Fmt.(list ~sep:(const string " -> ") Concrete_key.Boxed.pp)
        stack Concrete_key.pp key
      |> Result.error
    else if Key_set.mem visited key then Ok ()
    else (
      Key_set.set visited key ();
      Option.get rule |> Key_builder.dependencies |> go_all (bkey :: stack))
  and go_all stack = function
    | [] -> Ok ()
    | x :: xs -> (
      match go stack x with
      | Ok () -> go_all stack xs
      | Error _ as e -> e)
  in
  let rec go_seq s =
    match s () with
    | Seq.Nil -> Ok ()
    | Cons (x, xs) -> (
      match go [] x with
      | Ok () -> go_seq xs
      | Error _ as e -> e)
  in
  Builder_map.to_seq rules
  |> Seq.map (fun (Builder_map.Packed (k, _)) -> Concrete_key.box k)
  |> go_seq

type run_result = {
  total : int;
  changed : int;
  failed : int;
}

let apply ~env ?(progress = default_progress) ?(dry_run = false) (rules : Rules.rules) =
  match check_graph rules with
  | Error e -> Error e
  | Ok () ->
      let tasks =
        Builder_map.to_seq rules
        |> Seq.filter_map (fun (Builder_map.Packed (key, _)) ->
               match key with
               | Concrete_key.Resource _ -> Some (Concrete_key.BKey key)
               | Concrete_key.Var _ | Concrete_key.Machine _ -> None)
        |> CCSeq.to_rev_list
      in

      Switch.run @@ fun sw ->
      let keys = State_map.create (Builder_map.length rules) in
      let store = { keys; builders = rules; progress; sw; env; dry_run } in
      List.iter (start_rule ~store) tasks;
      await_rules ~store tasks |> ignore;
      let changed, failed =
        State_map.to_seq keys
        |> Seq.fold_left
             (fun (changed, failed) (State_map.Packed (_, result)) ->
               match result with
               | Finished (Ok (false, _)) -> (changed, failed)
               | Finished (Ok (true, _)) -> (changed + 1, failed)
               | Finished (Error _) | Pending _ -> (changed, failed + 1))
             (0, 0)
      in
      Ok { changed; failed; total = Builder_map.length rules }
