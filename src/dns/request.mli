type client

val create_client : sw:Eio.Switch.t -> clock:#Eio.Time.clock -> net:#Eio.Net.t -> client

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
