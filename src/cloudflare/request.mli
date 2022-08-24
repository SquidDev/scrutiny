type auth =
  | Token of string
  | Email of string * string

type client

val with_client : auth -> (client -> 'a Lwt.t) -> 'a Lwt.t

type request_body =
  | GET
  | DELETE
  | POST of string
  | PUT of string

val call :
  client:client ->
  ?query:(string * string list) list ->
  parse:(Yojson.Safe.t -> 'a) ->
  request_body ->
  string ->
  'a option Lwt.t
