module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

type auth =
  | Token of string
  | Email of string * string

let headers ~auth =
  let headers = Piaf.Headers.of_list [ ("Content-Type", "application/json") ] in
  match auth with
  | Token t -> Piaf.Headers.add headers "Authorization" ("Bearer " ^ t)
  | Email (email, tok) ->
      let headers = Piaf.Headers.add headers "X-Auth-Email" email in
      Piaf.Headers.add headers "X-Auth-Key" tok

type client = {
  client : Piaf.Client.t;
  headers : (string * string) list;
}

let with_client auth fn =
  match%lwt Uri.make ~scheme:"https" ~host:"api.cloudflare.com" () |> Piaf.Client.create with
  | Error e -> failwith (Piaf.Error.to_string e)
  | Ok client ->
      let c = { client; headers = headers ~auth |> Piaf.Headers.to_list } in
      Lwt.finalize (fun () -> fn c) (fun () -> Piaf.Client.shutdown client)

type message = {
  code : int;
  message : string;
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

let pp_msg out { code; message } = Format.fprintf out "Error in request [%d]: %s" code message

type 'a response = {
  success : bool;
  errors : message list;
  messages : message list;
  result : 'a option;
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

let mk_query path query = Uri.make ~path:("/client/v4/" ^ path) ~query () |> Uri.to_string

let make_request client ?body ~meth uri =
  match%lwt Piaf.Client.request client.client ?body ~headers:client.headers ~meth uri with
  | Error e -> Lwt.return_error e
  | Ok response -> (
      match%lwt Piaf.Body.to_string response.body with
      | Error e -> Lwt.return_error e
      | Ok body -> Lwt.return_ok (response, body))

let call ~client ?body ?(query = []) ~parse meth path =
  let uri = mk_query path query in
  match%lwt
    make_request client ?body:(Option.map (fun x -> Piaf.Body.of_string x) body) ~meth uri
  with
  | Error e ->
      Log.err (fun f -> f "Request failed: %a" Piaf.Error.pp_hum e);
      Lwt.return_none
  | Ok (response, body) -> (
      let json =
        match Yojson.Safe.from_string body |> response_of_yojson parse with
        | x ->
            List.iter (fun msg -> Log.warn (fun f -> f "%a" pp_msg msg)) x.messages;
            List.iter (fun msg -> Log.err (fun f -> f "%a" pp_msg msg)) x.errors;
            if x.success then x.result else None
        | exception Yojson.Json_error e ->
            Log.err (fun f -> f "Error decoding JSON %S => %s" body e);
            None
        | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, _) ->
            Log.err (fun f -> f "Error decoding JSON %S => %a" body Fmt.exn e);
            None
      in
      match response.status with
      | `OK | `Not_modified -> Lwt.return json
      | c ->
          Log.err (fun f -> f "Request failed: %s\n%S" (Piaf.Status.to_string c) body);
          Lwt.return_none)
