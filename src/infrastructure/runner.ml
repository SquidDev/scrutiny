open Core

type state =
  | Pending of (bool, unit) result Lwt.t
  | Finished of (bool, unit) result

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

(** Evaluate a rule, assuming all dependencies have been built. *)
let eval_rule ~(executor : Executor.t) ~dry_run ~store bkey =
  let (BKey (Resource { resource; key; value; user; dependencies = _ })) = bkey in
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
  Lwt.return (Result.map snd result)

let rec build_rule ~executor ~dry_run ~store bkey resolve =
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
    | Ok () -> eval_rule ~executor ~dry_run ~store bkey
    | Error () -> Lwt.return_error ()
  in

  KeyTbl.replace store.keys bkey (Finished result);
  Lwt.wakeup_later resolve result;
  store.key_done ();
  Lwt.return_unit

let default_progress _ = ()

let check_graph is_present rules =
  let visited = KeyTbl.create (List.length rules) in
  let rec go stack node =
    let (BKey (Resource { dependencies; key })) = node in
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

let apply ?(progress = default_progress) ?(executor = Executor.local) ?(dry_run = false)
    (rules : Rules.rules) =
  let key_done () = progress 1 in
  let keys = KeyTbl.create (List.length rules) in
  let store = { keys; key_done } in

  let tasks =
    List.rev_map
      (fun key ->
        let task, resolve = Lwt.wait () in
        KeyTbl.replace keys key (Pending task);
        fun () -> build_rule ~executor ~dry_run ~store key resolve)
      rules
  in
  match check_graph (KeyTbl.mem keys) rules with
  | Error e ->
      Logs.err (fun f -> f "%s" e);
      exit 1
  | Ok () ->
      let%lwt () = Lwt_list.iter_p (fun f -> f ()) tasks in
      let ok =
        KeyTbl.to_seq keys |> Seq.map snd
        |> CCSeq.for_all (function
             | Finished (Ok _) -> true
             | Finished (Error _) -> false)
      in
      if not ok then exit 1;
      Lwt.return_unit
