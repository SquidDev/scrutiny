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

  (** An {!Eio.Fiber.key} marking which key we're currently running. *)
  let context : t Eio.Fiber.key = Eio.Fiber.create_key ()

  let with_context key fn = Eio.Fiber.with_binding context key fn
end

let wrap_logger { Logs.report } =
  let report src level ~over k msgf =
    let key = Eio.Fiber.get PartialKey.context in
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
      apply : unit -> unit Or_exn.t;
    }

type t = {
  apply :
    'key 'value 'options.
    user:user ->
    ('key, 'value, 'options) Resource.t ->
    'key ->
    'value ->
    'options ->
    change Or_exn.t;
}

module LocalExecutor = struct
  let apply_basic (type key value option) ~env (resource : (key, value, option) Resource.t) key
      value option : change Or_exn.t =
    let module R = (val resource) in
    let wrap fn = PartialKey.with_context (PKey (resource, key)) @@ fun () -> Or_exn.run fn in
    let result = wrap (fun () -> R.apply ~env key value option) in
    let result =
      result
      |> Or_exn.map @@ function
         | Correct -> ECorrect
         | NeedsChange { diff; apply } -> ENeedsChange { diff; apply = (fun () -> wrap apply) }
    in
    result

  let mk ~env : t =
    let apply ~(user : user) resource key value option =
      match user with
      | `Current -> apply_basic ~env resource key value option
      | `Name _ | `Id _ -> Or_exn.Error "Cannot use different users on local executor."
    in
    { apply }
end

let local = LocalExecutor.mk
