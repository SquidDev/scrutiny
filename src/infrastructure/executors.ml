open Core

let local ~env =
  let open Executor in
  let apply (type key value option) (resource : (key, value, option) Resource.t) key value option :
      change Or_exn.t =
    let module R = (val resource) in
    let result = Or_exn.run (fun () -> R.apply ~env key value option) in
    let result =
      result
      |> Or_exn.map @@ function
         | Correct -> ECorrect
         | NeedsChange { diff; apply } ->
             ENeedsChange { diff; apply = (fun () -> Or_exn.run apply) }
    in
    result
  in

  let apply ~(user : user) resource key value option =
    match user with
    | `Current -> apply resource key value option
    | `Name _ | `Id _ -> Or_exn.Error "Cannot use different users on local executor."
  in
  { apply }
