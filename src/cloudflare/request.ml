module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

type auth =
  | Token of string
  | Email of string * string

let headers ~auth =
  let headers = [ ("Content-Type", "application/json") ] in
  match auth with
  | Token t -> ("Authorization", "Bearer " ^ t) :: headers
  | Email (email, tok) -> ("X-Auth-Key", tok) :: ("X-Auth-Email", email) :: headers

type client = { headers : (string * string) list }

let with_client auth fn = fn { headers = headers ~auth }

type request_body =
  | GET
  | DELETE
  | POST of string
  | PUT of string

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
  |> Uri.to_string

let read_function body =
  let idx = ref 0 in
  fun len ->
    let len = min len (String.length body - !idx) in
    let r = String.sub body !idx len in
    idx := !idx + len;
    r

let make_request client uri body =
  let request = Curl.init () in
  let buffer = Buffer.create 32 in
  Curl.set_url request uri;
  Curl.set_writefunction request (fun str -> Buffer.add_string buffer str; String.length str);
  List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v) client.headers |> Curl.set_httpheader request;

  (match body with
  | GET -> Curl.set_httpget request true
  | POST body ->
      Curl.set_post request true;
      Curl.set_readfunction request (read_function body)
  | DELETE -> Curl.set_customrequest request "DELETE"
  | PUT body ->
      Curl.set_put request true;
      Curl.set_readfunction request (read_function body));

  Lwt.finalize
    (fun () ->
      match%lwt Curl_lwt.perform request with
      | CURLE_OK -> Lwt.return_ok (Curl.get_responsecode request, Buffer.contents buffer)
      | err -> Lwt.return_error (Curl.strerror err))
    (fun () -> Curl.cleanup request; Lwt.return_unit)

let call ~client ?(query = []) ~parse body path =
  let uri = mk_query path query in
  match%lwt make_request client uri body with
  | Error e ->
      Log.err (fun f -> f "Request failed: %s" e);
      Lwt.return_none
  | Ok (status, body) -> (
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
      match status with
      | 200 | 201 -> Lwt.return json
      | c ->
          Log.err (fun f -> f "Request failed: %d\n%S" c body);
          Lwt.return_none)
