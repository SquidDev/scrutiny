(** Partial bindings for Cloudflare's API. *)

(** An authorisation method. *)
type auth =
  | Token of string
  | Email of string * string

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
  val find : auth:auth -> string -> id option Lwt.t
end

(** A single DNS record within a zone. *)
module DnsRecord : sig
  include Id

  type t =
    { id : id;
      type_ : string;  (** The type of this record (e.g. [A], [MX])*)
      name : string;  (** The domain name of this record (e.g. [example.com]). *)
      content : string;  (** The contents of this record (e.g. [127.0.0.1])*)
      proxiable : bool;  (** Whether this record {i can} be proxied through Cloudflare. *)
      proxied : bool;  (** Whether this record is proxied through Cloudflare. *)
      ttl : int;  (** The TTL of this record. *)
      locked : bool;  (** Whether this record can be edited. *)
      zone_id : Zone.id;  (** The zone this record belongs to. *)
      zone_name : string;  (** The name of the zone this record belongs to. *)
      created_on : string;  (** When this record was created. *)
      modified_on : string;  (** When this record was modified. *)
      data : Yojson.Safe.t option;  (** Additional information about this record. *)
      priority : int option  (** The priority of this record (only set for [MX] records).*)
    }

  (** List all records in a zone. Returns [None] on failure. *)
  val list : auth:auth -> zone:Zone.id -> t list option Lwt.t

  (** Create a new record, using the same fields as those defined in {!t}. Returns the created
      record, or [None] if an error occurred. *)
  val add :
    auth:auth ->
    zone:Zone.id ->
    type_:string ->
    name:string ->
    content:string ->
    ?proxied:bool ->
    ?ttl:int ->
    ?priority:int ->
    unit ->
    t option Lwt.t

  (** Edit an existing record, using the same fields as those defined in {!t}. Returns the updated
      record, or [None] if an error occurred. *)
  val update :
    auth:auth ->
    zone:Zone.id ->
    type_:string ->
    name:string ->
    content:string ->
    ?proxied:bool ->
    ?ttl:int ->
    id ->
    t option Lwt.t

  (** Delete a record. Returns [None] if an error occurred. *)
  val delete : auth:auth -> zone:Zone.id -> id -> unit option Lwt.t

  (** A declaration of a DNS record. *)
  module Spec : sig
    type t

    val pp : Format.formatter -> t -> unit

    (** A TXT record. *)
    val txt : ?ttl:int -> name:string -> string -> t

    (** An A record optionally proxied through Cloudflare (defaults to false). *)
    val a : ?ttl:int -> ?proxied:bool -> name:string -> string -> t

    (** An AAAA record optionally proxied through Cloudflare (defaults to false). *)
    val aaaa : ?ttl:int -> ?proxied:bool -> name:string -> string -> t

    (** An CNAME record optionally proxied through Cloudflare (defaults to false). *)
    val cname : ?ttl:int -> ?proxied:bool -> name:string -> string -> t

    (** An MX record with an optional priority. *)
    val mx : ?ttl:int -> ?priority:int -> name:string -> string -> t

    (** Compare the current list of DNS records in a zone with a desired/expected list. Returns the
        modified record, as a well as a result marking success. *)
    val sync :
      ?dryrun:bool ->
      auth:auth ->
      zone:Zone.id ->
      t list ->
      ((unit, string) result * Scrutiny_diff.t) Lwt.t
  end
end
