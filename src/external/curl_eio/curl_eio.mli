type t

(** Create a new client. *)
val create : sw:Eio.Switch.t -> clock:[> ] Eio.Time.clock -> net:[> ] Eio.Net.t -> t

(** Run a request using this client. *)
val perform : t -> Curl.t -> Curl.curlCode
