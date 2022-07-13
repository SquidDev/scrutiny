type auth =
  | Token of string
  | Email of string * string

type client

val with_client : auth -> (client -> 'a Lwt.t) -> 'a Lwt.t

val call :
  client:client ->
  ?body:string ->
  ?query:(string * string list) list ->
  parse:(Yojson.Safe.t -> 'a) ->
  Piaf.Method.t ->
  string ->
  'a option Lwt.t
