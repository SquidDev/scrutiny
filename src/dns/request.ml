type client = Curl_eio.t

let create_client = Curl_eio.create

type request_body =
  | GET
  | DELETE
  | POST of string
  | PUT of string

let read_function body =
  let idx = ref 0 in
  fun len ->
    let len = min len (String.length body - !idx) in
    let r = String.sub body !idx len in
    idx := !idx + len;
    r

let call_impl ~client ~headers body uri =
  let request = Curl.init () in
  let buffer = Buffer.create 32 in
  Curl.set_url request (Uri.to_string uri);
  Curl.set_writefunction request (fun str -> Buffer.add_string buffer str; String.length str);
  List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v) headers |> Curl.set_httpheader request;

  (match body with
  | GET -> Curl.set_httpget request true
  | POST body ->
      Curl.set_post request true;
      Curl.set_readfunction request (read_function body)
  | DELETE -> Curl.set_customrequest request "DELETE"
  | PUT body ->
      Curl.set_put request true;
      Curl.set_readfunction request (read_function body));

  Fun.protect ~finally:(fun () -> Curl.cleanup request) @@ fun () ->
  match Curl_eio.perform client request with
  | CURLE_OK -> Ok (Curl.get_responsecode request, Buffer.contents buffer)
  | err -> Error (Curl.strerror err)

let call ~client ~headers body uri =
  match call_impl ~client ~headers body uri with
  | Error e -> Error e
  | Ok (status, body) -> (
    match status with
    | 200 | 201 -> Ok body
    | c -> Printf.sprintf "Request failed with status %d:\n%s" c body |> Result.error)
