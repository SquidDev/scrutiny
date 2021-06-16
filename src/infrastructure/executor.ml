open Core

type change =
  | ECorrect
  | ENeedsChange of
      { diff : Scrutiny_diff.t;
        apply : unit -> unit Or_exn.t Lwt.t
      }

type t =
  { apply :
      'key 'value 'options.
      user:user ->
      ('key, 'value, 'options) Resource.t ->
      'key ->
      'value ->
      'options ->
      change Or_exn.t Lwt.t
  }

module LocalExecutor = struct
  let apply_basic (type key value option) (resource : (key, value, option) Resource.t) key value
      option : change Or_exn.t Lwt.t =
    let module R = (val resource) in
    let%lwt result = Or_exn.run_lwt (fun () -> R.apply key value option) in
    let result =
      result
      |> Or_exn.map @@ function
         | Correct -> ECorrect
         | NeedsChange { diff; apply } ->
             ENeedsChange { diff; apply = (fun () -> Or_exn.run_lwt apply) }
    in
    Lwt.return result

  let apply ~(user : user) resource key value option =
    match user with
    | `Current -> apply_basic resource key value option
    | `Name _ | `Id _ -> Lwt.return (Or_exn.Error "Cannot use different users on local executor.")
end

let local : t = { apply = LocalExecutor.apply }
