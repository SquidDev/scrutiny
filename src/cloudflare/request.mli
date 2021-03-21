type auth =
  | Token of string
  | Email of string * string

val call :
  ?body:string ->
  ?query:(string * string list) list ->
  parse:(Yojson.Safe.t -> 'a) ->
  auth:auth ->
  Cohttp.Code.meth ->
  string ->
  'a option Lwt.t
