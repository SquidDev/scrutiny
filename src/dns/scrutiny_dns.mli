(** Partial bindings for Cloudflare's API. *)

(** The DNS provider to use. *)
type source =
  | Porkbun of {
      api_key : string;
      secret : string;
    }

module Client : sig
  type t

  val create : sw:Eio.Switch.t -> clock:Eio.Time.clock -> net:Eio.Net.t -> source -> t
end

(** Represents a unique ID of some resource. This signature is included in the definitions of actual
    resources. *)
module type Id = sig
  type id

  val pp_id : Format.formatter -> id -> unit
end

(** A "zone", representing some domain name on Cloudflare. *)
module Zone : sig
  include Id

  (** Find a zone. Returns [None] on failure. *)
  val find : client:Client.t -> string -> (id, string) result
end

(** A single DNS record within a zone. *)
module DnsRecord : sig
  include Id

  type t = {
    id : id;
    type_ : string;  (** The type of this record (e.g. [A], [MX])*)
    name : string;  (** The domain name of this record (e.g. [example.com]). *)
    content : string;  (** The contents of this record (e.g. [127.0.0.1])*)
    ttl : int;  (** The TTL of this record. *)
    zone_id : Zone.id;  (** The zone this record belongs to. *)
    priority : int option;  (** The priority of this record (only set for [MX] records).*)
  }

  (** List all records in a zone. Returns [None] on failure. *)
  val list : client:Client.t -> zone:Zone.id -> (t list, string) result

  (** Create a new record, using the same fields as those defined in {!t}. Returns the created
      record's ID. *)
  val add :
    client:Client.t ->
    zone:Zone.id ->
    type_:string ->
    name:string ->
    content:string ->
    ?ttl:int ->
    ?priority:int ->
    unit ->
    (id, string) result

  (** Edit an existing record, using the same fields as those defined in {!t}. *)
  val update :
    client:Client.t ->
    zone:Zone.id ->
    type_:string ->
    name:string ->
    content:string ->
    ?ttl:int ->
    ?priority:int ->
    id ->
    (unit, string) result

  (** Delete a record. Returns [None] if an error occurred. *)
  val delete : client:Client.t -> zone:Zone.id -> id -> (unit, string) result

  (** A declaration of a DNS record. *)
  module Spec : sig
    type t

    val pp : Format.formatter -> t -> unit

    (** A TXT record. *)
    val txt : ?ttl:int -> name:string -> string -> t

    (** An A record. *)
    val a : ?ttl:int -> name:string -> string -> t

    (** An AAAA record. *)
    val aaaa : ?ttl:int -> name:string -> string -> t

    (** An CNAME record. *)
    val cname : ?ttl:int -> name:string -> string -> t

    (** An MX record with an optional priority. *)
    val mx : ?ttl:int -> ?priority:int -> name:string -> string -> t

    (** A CAA record. *)
    val caa : ?ttl:int -> name:string -> string -> t

    (** Compare the current list of DNS records in a zone with a desired/expected list. Returns the
        modified record, as a well as a result marking success. *)
    val sync :
      ?dryrun:bool ->
      client:Client.t ->
      zone:Zone.id ->
      t list ->
      (unit, string) result * Scrutiny_diff.t
  end
end
