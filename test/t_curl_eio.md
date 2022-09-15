## Curl_eio
The `curl_eio` module provides an eio-compatible wrapper over curl, allowing you to make asynchronous http requests.

First, initialise some logging:

```ocaml
# Printexc.record_backtrace true;
  Logs.Src.list () |> List.iter (fun src ->
    match Logs.Src.name src with
    | "Curl_eio" -> Logs.Src.set_level src (Some Debug)
    | _ -> ());
  Logs.format_reporter () |> Logs.set_reporter ;;
- : unit = ()
```

Now, we define a helper function to make a get request.

```ocaml
let get ~client url =
  let conn = Curl.init () in
  Curl.set_url conn url;
  Curl.set_httpget conn true;
  let buffer = Buffer.create 32 in
  Curl.set_writefunction conn (fun str -> Buffer.add_string buffer str; String.length str);

  Fun.protect ~finally:(fun () -> Curl.cleanup conn) @@ fun () ->
  match Curl_eio.perform client conn with
  | CURLE_OK -> Ok (Curl.get_responsecode conn, Buffer.contents buffer)
  | err -> Error (Curl.strerror err)
```

We can make a simple http request.

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = Curl_eio.create ~sw ~clock:env#clock ~net:env#net in
  get ~client "https://httpbin.org/base64/SGVsbG8sIHdvcmxkIQ==" ;;
mdx_gen.bc: [DEBUG] Starting request to https://httpbin.org/base64/SGVsbG8sIHdvcmxkIQ==
mdx_gen.bc: [DEBUG] Finished request to https://httpbin.org/base64/SGVsbG8sIHdvcmxkIQ==
- : (int * string, string) result = Ok (200, "Hello, world!")
```

We can make HTTP requests in parallel.

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = Curl_eio.create ~sw ~clock:env#clock ~net:env#net in
  let start = Eio.Time.now env#clock in
  Eio.Fiber.pair
    (fun () -> get ~client "https://httpbin.org/delay/3")
    (fun () -> get ~client "https://httpbin.org/delay/3")
  |> ignore;
  assert (Eio.Time.now env#clock -. start < 6.) ;;
mdx_gen.bc: [DEBUG] Starting request to https://httpbin.org/delay/3
mdx_gen.bc: [DEBUG] Starting request to https://httpbin.org/delay/3
mdx_gen.bc: [DEBUG] Finished request to https://httpbin.org/delay/3
mdx_gen.bc: [DEBUG] Finished request to https://httpbin.org/delay/3
- : unit = ()
```
