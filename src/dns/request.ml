type client = {
  sw : Eio.Switch.t;
  env : Eio_unix.Stdenv.base;
  mutable client : (Uri.t * Piaf.Client.t) option;
}

let create_client ~sw ~env = { sw; env; client = None }

type request_body =
  | GET
  | DELETE
  | POST of string
  | PUT of string

let call_impl ~client ~headers body uri =
  let ( let* ) = Result.bind in
  let* piaf =
    match client.client with
    | Some (uri', piaf)
      when Option.equal String.equal (Uri.scheme uri) (Uri.scheme uri')
           && Option.equal String.equal (Uri.host uri) (Uri.host uri')
           && Option.equal Int.equal (Uri.port uri) (Uri.port uri') -> Ok piaf
    | Some (_, piaf) ->
        Piaf.Client.shutdown piaf;
        client.client <- None;
        Piaf.Client.create ~sw:client.sw client.env uri
    | None -> Piaf.Client.create ~sw:client.sw client.env uri
  in
  client.client <- Some (uri, piaf);

  let path = Uri.path uri in
  let* response =
    match body with
    | GET -> Piaf.Client.get piaf ~headers path
    | DELETE -> Piaf.Client.delete piaf ~headers path
    | POST body -> Piaf.Client.post piaf ~headers ~body:(Piaf.Body.of_string body) path
    | PUT body -> Piaf.Client.put piaf ~headers ~body:(Piaf.Body.of_string body) path
  in

  let* body = Piaf.Body.to_string response.body in
  Ok (Piaf.Status.to_code response.status, body)

let call ~client ~headers body uri =
  match call_impl ~client ~headers body uri with
  | Error e -> Error (Piaf.Error.to_string e)
  | Ok (status, body) -> (
    match status with
    | 200 | 201 -> Ok body
    | c -> Printf.sprintf "Request failed with status %d:\n%s" c body |> Result.error)
