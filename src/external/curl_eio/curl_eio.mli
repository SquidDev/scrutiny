type t

(** Create a new client. *)
val create : env:< clock : #Eio.Time.clock ; net : #Eio.Net.t ; .. > -> sw:Eio.Switch.t -> unit -> t

(** Run a request using this client. *)
val perform : t -> Curl.t -> Curl.curlCode
