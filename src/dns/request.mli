type client

val create_client : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> client

type request_body =
  | GET
  | DELETE
  | POST of string
  | PUT of string

val call :
  client:client ->
  headers:(string * string) list ->
  request_body ->
  Uri.t ->
  (string, string) result
