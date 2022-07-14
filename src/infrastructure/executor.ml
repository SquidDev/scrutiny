open Core

module PartialKey = struct
  type t = PKey : ('key, 'value, 'options) Core.Resource.t * 'key -> t

  let hash (PKey ((module R), key)) = R.Key.hash key

  let equal (PKey ((module R1), key1)) (PKey ((module R2), key2)) =
    if R1.id = R2.id then R1.Key.equal key1 (Obj.magic key2) else false

  let tag =
    let pp_key out = function
      | PKey (resource, key) ->
          let module R = (val resource) in
          R.pp out key
    in
    Logs.Tag.def "BoxedKey" pp_key

  (** An {!Lwt.key} marking which key we're currently running. While this API is deprecated, it's
      the only way to do this :(. *)
  let context : t Lwt.key = Lwt.new_key ()

  let with_context key fn = Lwt.with_value context (Some key) fn
end

let wrap_logger { Logs.report } =
  let report src level ~over k msgf =
    let key = Lwt.get PartialKey.context in
    report src level ~over k @@ fun f ->
    msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
    let tags =
      if Logs.Tag.mem PartialKey.tag tags then tags
      else
        match key with
        | Some key -> Logs.Tag.add PartialKey.tag key tags
        | None -> tags
    in
    f ?header ~tags fmt
  in
  { Logs.report }

type change =
  | ECorrect
  | ENeedsChange of {
      diff : Scrutiny_diff.t;
      apply : unit -> unit Or_exn.t Lwt.t;
    }

type t = {
  apply :
    'key 'value 'options.
    user:user ->
    ('key, 'value, 'options) Resource.t ->
    'key ->
    'value ->
    'options ->
    change Or_exn.t Lwt.t;
}

module LocalExecutor = struct
  let apply_basic (type key value option) (resource : (key, value, option) Resource.t) key value
      option : change Or_exn.t Lwt.t =
    let module R = (val resource) in
    let wrap fn = PartialKey.with_context (PKey (resource, key)) @@ fun () -> Or_exn.run_lwt fn in
    let%lwt result = wrap (fun () -> R.apply key value option) in
    let result =
      result
      |> Or_exn.map @@ function
         | Correct -> ECorrect
         | NeedsChange { diff; apply } -> ENeedsChange { diff; apply = (fun () -> wrap apply) }
    in
    Lwt.return result

  let apply ~(user : user) resource key value option =
    match user with
    | `Current -> apply_basic resource key value option
    | `Name _ | `Id _ -> Lwt.return (Or_exn.Error "Cannot use different users on local executor.")
end

let local : t = { apply = LocalExecutor.apply }
