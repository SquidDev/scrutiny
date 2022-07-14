open Core
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

module RemoteTbl = Hashtbl.Make (struct
  type t = Remote.t

  let hash = Hashtbl.hash
  let equal = ( == )
end)

type state =
  | Pending of (bool, unit) result Lwt.t
  | Finished of (bool, unit) result

type progress = {
  key_start : boxed_key -> unit;
  key_done : boxed_key -> unit;
}

let default_progress = { key_start = (fun _ -> ()); key_done = (fun _ -> ()) }

type t = {
  keys : state KeyTbl.t;
  progress : progress;
  executors : Executor.t Or_exn.t Lwt.t RemoteTbl.t;
  switch : Lwt_switch.t option;
}

(** Evaluate an action, assuming all dependencies have been built.

    This returns the resulting value, the union of all changed edges, and whether any incoming edge
    changed. *)
let eval_action (type key value options) ~(resource : (key, value, options) Resource.t) ~(store : t)
    x =
  let module R = (val resource) in
  let rec go : type ret. (ret, options) action -> (ret * options * bool, unit) result = function
    | Pure v -> Ok (v, R.EdgeOptions.default, false)
    | Need (options, (Resource _ as key)) -> (
      match KeyTbl.find store.keys (BKey key) with
      | Pending _ -> failwith "Impossible: Key not ready"
      | Finished (Error ()) -> Error ()
      | Finished (Ok changed) ->
          Ok ((), (if changed then options else R.EdgeOptions.default), changed))
    | Bind (x, f) -> go x |> Result.map (fun (res, options, changed) -> (f res, options, changed))
    | Pair (l, r) -> (
      match (go l, go r) with
      | Ok (l, l_options, l_changed), Ok (r, r_options, r_changed) ->
          Ok ((l, r), R.EdgeOptions.union l_options r_options, l_changed || r_changed)
      | Error (), _ | _, Error () -> Error ())
  in
  match go x with
  | Error () -> Lwt.return_error ()
  | Ok (value, options, changed) ->
      let%lwt value = value in
      Lwt.return_ok (value, options, changed)

let get_executor ~store:{ executors; switch; _ } bkey =
  let (BKey (Resource { machine; _ })) = bkey in
  match machine with
  | Local -> Lwt.return (Or_exn.Ok Executor.local)
  | Remote remote -> (
    match RemoteTbl.find_opt executors remote with
    | Some x -> x
    | None ->
        let tunnel = Or_exn.run_lwt (fun () -> Tunnel.ssh ?switch remote) in
        RemoteTbl.replace executors remote tunnel;
        tunnel)

(** Evaluate a rule, assuming all dependencies have been built. *)
let eval_rule ~dry_run ~store bkey =
  let (BKey (Resource { resource; key; value; user; _ })) = bkey in
  let module R = (val resource) in
  let%lwt result =
    Executor.PartialKey.with_context (PKey (resource, key)) @@ fun () ->
    (* Attempt to evaluate the rule. This is 90% error handling and 10% actual code. Sorry! *)
    match%lwt eval_action ~resource ~store value with
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
          get_executor ~store bkey <?> ("Failed to get executor", "Exception getting executor")
        in
        let>> result =
          executor.apply ~user resource key value options
          <?> ("Failed to compute changes", "Exception computing changes")
        in
        match result with
        | ECorrect ->
            Log.debug (fun f -> f "No changes needed");
            Lwt.return_ok (value, false)
        | ENeedsChange { diff; apply } ->
            Log.info (fun f -> f "Applying %a:@\n%a" R.pp key (Scrutiny_diff.pp ~full:false) diff);
            if dry_run then Lwt.return_ok (value, true)
            else
              let>> () = apply () <?> ("Failed to apply changes", "Exception applying changes") in
              Lwt.return_ok (value, true))
  in
  Lwt.return (Result.map snd result)

let build_rule ~dry_run ~store bkey resolve =
  let (BKey (Resource { dependencies; _ })) = bkey in
  let rec eval_deps = function
    | [] -> Lwt.return_ok ()
    | dep :: deps -> (
        let%lwt result =
          match KeyTbl.find store.keys dep with
          | Finished f -> Lwt.return f
          | Pending x -> x
        in
        match result with
        | Ok _ -> eval_deps deps
        | Error _ -> Lwt.return_error ())
  in

  let%lwt result =
    match%lwt eval_deps dependencies with
    | Ok () -> store.progress.key_start bkey; eval_rule ~dry_run ~store bkey
    | Error () -> Lwt.return_error ()
  in

  KeyTbl.replace store.keys bkey (Finished result);
  Lwt.wakeup_later resolve result;
  store.progress.key_done bkey;
  Lwt.return_unit

let check_graph is_present rules =
  let visited = KeyTbl.create (List.length rules) in
  let rec go stack node =
    let (BKey (Resource { dependencies; _ })) = node in
    if KeyTbl.mem visited node then Ok ()
    else if not (is_present node) then Error "Missing node in rule list"
    else if List.memq node stack then Error "Loop in dependency graph"
    else (
      KeyTbl.replace visited node true;
      go_all (node :: stack) dependencies)
  and go_all stack = function
    | [] -> Ok ()
    | x :: xs -> (
      match go stack x with
      | Ok () -> go_all stack xs
      | Error _ as e -> e)
  in
  go_all [] rules

type run_result = {
  total : int;
  changed : int;
  failed : int;
}

let apply ?(progress = default_progress) ?switch ?(dry_run = false) (rules : Rules.rules) =
  let total = List.length rules in
  let keys = KeyTbl.create total in
  let store = { keys; progress; executors = RemoteTbl.create 2; switch } in

  let tasks =
    List.rev_map
      (fun key ->
        let task, resolve = Lwt.wait () in
        KeyTbl.replace keys key (Pending task);
        fun () -> build_rule ~dry_run ~store key resolve)
      rules
  in
  match check_graph (KeyTbl.mem keys) rules with
  | Error e -> Lwt.return_error e
  | Ok () ->
      let%lwt () = Lwt_list.iter_p (fun f -> f ()) tasks in
      let changed, failed =
        KeyTbl.fold
          (fun _ result (changed, failed) ->
            match result with
            | Finished (Ok false) -> (changed, failed)
            | Finished (Ok true) -> (changed + 1, failed)
            | Finished (Error _) | Pending _ -> (changed, failed + 1))
          keys (0, 0)
      in
      Lwt.return_ok { changed; failed; total = List.length rules }
