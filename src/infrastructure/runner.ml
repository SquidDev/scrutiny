open Core
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

module Key_set =
  Het_map.Make
    (Concrete_key)
    (struct
      type 'a t = unit
    end)

module RemoteTbl = Hashtbl.Make (struct
  type t = Remote.t

  let hash = Hashtbl.hash
  let equal = ( == )
end)

type 'a state =
  | Pending of (bool * 'a, unit) result Lwt.t
  | Finished of (bool * 'a, unit) result

module Packed_state = struct
  type 'a t = State : 'a state -> ('a * 'e) t [@@unboxed]
end

module State_map = Het_map.Make (Concrete_key) (Packed_state)

type progress = {
  key_start : Core.Concrete_key.boxed -> unit;
  key_done : Core.Concrete_key.boxed -> unit;
}

let default_progress = { key_start = (fun _ -> ()); key_done = (fun _ -> ()) }

type t = {
  keys : State_map.t;
  progress : progress;
  executors : Executor.t Or_exn.t Lwt.t RemoteTbl.t;
  switch : Lwt_switch.t option;
  local_executor : Executor.t;
}

(** Evaluate an action, assuming all dependencies have been built.

    This returns the resulting value, the union of all changed edges, and whether any incoming edge
    changed. *)
let eval_action (type options) ~options:(module E : Core.EdgeOptions with type t = options)
    ~(store : t) unpack x =
  let rec go : type ret. (ret, options) action -> (ret * options * bool, unit) result = function
    | Pure v -> Ok (v, E.default, false)
    | Need (options, key) -> (
        let (State state) = State_map.get_exn store.keys key in
        match state with
        | Pending _ -> failwith "Impossible: Key not ready"
        | Finished (Error ()) -> Error ()
        | Finished (Ok (changed, ret)) -> Ok (ret, (if changed then options else E.default), changed)
        )
    | Bind (x, f) -> go x |> Result.map (fun (res, options, changed) -> (f res, options, changed))
    | Pair (l, r) -> (
      match (go l, go r) with
      | Ok (l, l_options, l_changed), Ok (r, r_options, r_changed) ->
          Ok ((l, r), E.union l_options r_options, l_changed || r_changed)
      | Error (), _ | _, Error () -> Error ())
  in
  match go x with
  | Error () -> Lwt.return_error ()
  | Ok (value, options, changed) ->
      let%lwt value = unpack value in
      Lwt.return_ok (value, options, changed)

let get_executor ~store:{ executors; switch; local_executor; _ } { Concrete_resource.machine; _ } =
  match machine with
  | Local -> Lwt.return (Or_exn.Ok local_executor)
  | Remote remote -> (
    match RemoteTbl.find_opt executors remote with
    | Some x -> x
    | None ->
        let tunnel = Or_exn.run_lwt (fun () -> Tunnel.ssh ?switch remote) in
        RemoteTbl.replace executors remote tunnel;
        tunnel)

(** Evaluate a rule, assuming all dependencies have been built. *)
let eval_resource (type key value options) ~dry_run ~store
    (cresource : (key, value, options) Concrete_resource.t) builder =
  let { Concrete_resource.resource; key; user; _ } = cresource in
  let value = builder.term in
  let module R = (val resource) in
  let%lwt result =
    Executor.PartialKey.with_context (PKey (resource, key)) @@ fun () ->
    (* Attempt to evaluate the rule. This is 90% error handling and 10% actual code. Sorry! *)
    match%lwt eval_action ~options:(module R.EdgeOptions) ~store Fun.id value with
    | Error () -> Lwt.return_error ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Log.err (fun f -> f "Exception getting value for %a: %a" R.pp key Fmt.exn_backtrace (e, bt));
        Lwt.return_error ()
    | Ok (value, options, _changed) -> (
        Log.debug (fun f -> f "Applying: %a" R.pp key);
        let ( let>> ) = Lwt_result.bind in
        let ( <?> ) action (err, exn) = Or_exn.log (module Log) action (err, exn) in
        let>> executor =
          get_executor ~store cresource <?> ("Failed to get executor", "Exception getting executor")
        in
        let>> result =
          executor.apply ~user resource key value options
          <?> ("Failed to compute changes", "Exception computing changes")
        in
        match result with
        | ECorrect ->
            Log.debug (fun f -> f "No changes needed");
            Lwt.return_ok (false, ())
        | ENeedsChange { diff; apply } ->
            Log.info (fun f -> f "Applying %a:@\n%a" R.pp key (Scrutiny_diff.pp ~full:false) diff);
            if dry_run then Lwt.return_ok (true, ())
            else
              let>> () = apply () <?> ("Failed to apply changes", "Exception applying changes") in
              Lwt.return_ok (true, ()))
  in
  Lwt.return result

let eval_var ~store var value =
  match%lwt eval_action ~options:(module Types.Unit) ~store Lwt.all value.term with
  | Error () -> Lwt.return_error ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Log.err (fun f ->
          f "Exception getting value for %a: %a" Concrete_var.pp var Fmt.exn_backtrace (e, bt));
      Lwt.return_error ()
  | Ok (value, _options, _changed) -> Lwt.return_ok (false, value)

let build_rule (type r opts) ~dry_run ~store (key : (r * opts) Concrete_key.t)
    (builder : (r * opts) Key_builder.t) resolve =
  let dependencies = Key_builder.dependencies builder in
  let rec eval_deps = function
    | [] -> Lwt.return_ok ()
    | Concrete_key.BKey dep :: deps -> (
        let (State state) = State_map.get_exn store.keys dep in
        let%lwt result =
          match state with
          | Finished f -> Lwt.return f
          | Pending x -> x
        in
        match result with
        | Ok _ -> eval_deps deps
        | Error _ -> Lwt.return_error ())
  in

  (* Written in weird CPS style to work with LWT's desugaring *)
  let finish (result : (bool * r, unit) result) =
    State_map.set store.keys key (Packed_state.State (Finished result));
    Lwt.wakeup_later resolve result;
    store.progress.key_done (BKey key);
    Lwt.return_unit
  in

  match%lwt eval_deps dependencies with
  | Ok () -> (
      store.progress.key_start (BKey key);
      match (key, builder) with
      | Resource key, Resource builder ->
          let%lwt res = eval_resource ~dry_run ~store key builder in
          finish res
      | Var key, Var builder ->
          let%lwt res = eval_var ~store key builder in
          finish res)
  | Error () -> finish (Error ())

let check_graph rules =
  let visited = Key_set.create (Builder_map.length rules) in
  let rec go stack (BKey key as bkey : Concrete_key.boxed) =
    if not (Builder_map.mem rules key) then Error "Missing node in rule list"
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
      Builder_map.get_exn rules key |> Key_builder.dependencies |> go_all (bkey :: stack))
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
  |> Seq.map (fun (Builder_map.Packed (k, _)) -> Concrete_key.BKey k)
  |> go_seq

type run_result = {
  total : int;
  changed : int;
  failed : int;
}

let apply ~env ?(progress = default_progress) ?switch ?(dry_run = false) (rules : Rules.rules) =
  let total = Builder_map.length rules in
  let keys = State_map.create total in
  let store =
    { keys; progress; executors = RemoteTbl.create 2; switch; local_executor = Executor.local ~env }
  in

  (* Nasty indirection to cope with our definition of Concrete_key. *)
  let add_task key builder =
    let task, resolve = Lwt.wait () in
    State_map.set keys key (State (Pending task));
    fun () -> build_rule ~dry_run ~store key builder resolve
  in
  let tasks =
    Builder_map.to_seq rules
    |> Seq.map (fun (Builder_map.Packed (key, builder)) ->
           match key with
           | Concrete_key.Resource _ -> add_task key builder
           | Concrete_key.Var _ -> add_task key builder)
    |> CCSeq.to_rev_list
  in
  match check_graph rules with
  | Error e -> Lwt.return_error e
  | Ok () ->
      let%lwt () = Lwt_list.iter_p (fun f -> f ()) tasks in
      let changed, failed =
        State_map.to_seq keys
        |> Seq.fold_left
             (fun (changed, failed) (State_map.Packed (_, State result)) ->
               match result with
               | Finished (Ok (false, _)) -> (changed, failed)
               | Finished (Ok (true, _)) -> (changed + 1, failed)
               | Finished (Error _) | Pending _ -> (changed, failed + 1))
             (0, 0)
      in
      Lwt.return_ok { changed; failed; total = Builder_map.length rules }
