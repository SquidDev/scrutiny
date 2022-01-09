module Client = Cohttp_lwt_unix.Client
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

type auth =
  | Token of string
  | Email of string * string

let headers ~auth =
  let headers = Cohttp.Header.of_list [ ("Content-Type", "application/json") ] in
  match auth with
  | Token t -> Cohttp.Header.add headers "Authorization" ("Bearer " ^ t)
  | Email (email, tok) ->
      let headers = Cohttp.Header.add headers "X-Auth-Email" email in
      Cohttp.Header.add headers "X-Auth-Key" tok

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

let mk_query path query =
  Uri.make ~scheme:"https" ~host:"api.cloudflare.com" ~path:("/client/v4/" ^ path) ~query ()

let call ?body ?(query = []) ~parse ~auth meth path =
  let open Lwt.Syntax in
  let uri = mk_query path query in
  let* response, body =
    Client.call ?body:(Option.map (fun x -> `String x) body) ~headers:(headers ~auth) meth uri
  in
  let+ body = Cohttp_lwt.Body.to_string body in
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
  match Cohttp.Response.status response with
  | `OK | `Not_modified -> json
  | c ->
      Log.err (fun f -> f "Request failed: %s\n%S" (Cohttp.Code.string_of_status c) body);
      None
