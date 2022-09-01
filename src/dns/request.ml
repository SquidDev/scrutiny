type client = unit

let with_client fn = fn ()

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

let call_impl ~headers body uri =
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

  Lwt.finalize
    (fun () ->
      match%lwt Curl_lwt.perform request with
      | CURLE_OK -> Lwt.return_ok (Curl.get_responsecode request, Buffer.contents buffer)
      | err -> Lwt.return_error (Curl.strerror err))
    (fun () -> Curl.cleanup request; Lwt.return_unit)

let call ~client:() ~headers body uri =
  match%lwt call_impl ~headers body uri with
  | Error e -> Lwt.return_error e
  | Ok (status, body) -> (
    match status with
    | 200 | 201 -> Lwt.return_ok body
    | c -> Printf.sprintf "Request failed with status %d:\n%s" c body |> Lwt.return_error)
