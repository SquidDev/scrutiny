open Core

type state =
  | NotStarted
  | WaitingDep of boxed_key
  | Finished : (bool, unit) result -> state

type t =
  { keys : state KeyTbl.t;
    key_done : unit -> unit
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
      | NotStarted | WaitingDep _ -> failwith "Impossible: Key not ready"
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

(** Evaluate a rule, assuming all dependencies have been built. *)
let eval_rule ~(executor : Executor.t) ~dry_run ~store boxed_key =
  let (BKey (Resource { resource; key; value; user; dependencies = _ })) = boxed_key in
  let module R = (val resource) in
  let logger = Logs.src_log (Logs.Src.create (Format.asprintf "%a" R.pp key)) in
  let module Log = (val logger) in
  let%lwt result =
    (* Attempt to evaluate the rule. This is 90% error handling and 10% actual code. Sorry! *)
    match%lwt eval_action ~resource ~store value with
    | Error () -> Lwt.return_error ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Log.err (fun f -> f "Exception getting value for %a: %a" R.pp key Fmt.exn_backtrace (e, bt));
        Lwt.return_error ()
    | Ok (value, options, _changed) -> (
        Log.info (fun f -> f "Applying: %a" R.pp key);
        match%lwt executor.apply ~user resource key value options with
        | Error e ->
            Log.err (fun f -> f "Failed to compute changes: %s" e);
            Lwt.return_error ()
        | Exception e ->
            Log.err (fun f -> f "Exception computing changes: %s" e);
            Lwt.return_error ()
        | Ok ECorrect ->
            Log.debug (fun f -> f "No changes needed");
            Lwt.return_ok (value, false)
        | Ok (ENeedsChange { diff; apply }) -> (
            if dry_run then (
              Log.info (fun f -> f "%a" (Scrutiny_diff.pp ~full:false) diff);
              Lwt.return_ok (value, true))
            else
              match%lwt apply () with
              | Ok () ->
                  Log.info (fun f -> f "%a" (Scrutiny_diff.pp ~full:false) diff);
                  Lwt.return_ok (value, true)
              | Error e ->
                  Log.err (fun f -> f "Failed to apply changes: %s" e);
                  Lwt.return_error ()
              | Exception e ->
                  Log.err (fun f -> f "Exception applying changes: %s" e);
                  Lwt.return_error ()))
  in
  KeyTbl.replace store.keys boxed_key (Finished (Result.map snd result));
  store.key_done ();
  Lwt.return_unit

let rec build_rule ~executor ~dry_run ~store bkey =
  match KeyTbl.find store.keys bkey with
  | Finished _ -> Lwt.return_unit
  | WaitingDep _ -> failwith "Loop in dependency graph"
  | NotStarted ->
      let (BKey (Resource { dependencies; _ })) = bkey in
      let%lwt () =
        dependencies
        |> Lwt_list.iter_s @@ fun d ->
           KeyTbl.replace store.keys bkey (WaitingDep d);
           build_rule ~executor ~dry_run ~store d
      in
      eval_rule ~executor ~dry_run ~store bkey

let default_progress _ = ()

let apply ?(progress = default_progress) ?(executor = Executor.local) ?(dry_run = false)
    (rules : Rules.rules) =
  let key_done () = progress 1 in
  let keys = KeyTbl.create (List.length rules) in
  List.iter (fun key -> KeyTbl.replace keys key NotStarted) rules;
  let store = { keys; key_done } in
  let%lwt () = Lwt_list.iter_s (build_rule ~executor ~dry_run ~store) rules in
  let ok =
    KeyTbl.to_seq keys |> Seq.map snd
    |> CCSeq.for_all (function
         | NotStarted | WaitingDep _ | Finished (Ok _) -> true
         | Finished (Error _) -> false)
  in
  if not ok then exit 1;
  Lwt.return_unit
