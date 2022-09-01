type client

val with_client : (client -> 'a Lwt.t) -> 'a Lwt.t

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
  (string, string) result Lwt.t
