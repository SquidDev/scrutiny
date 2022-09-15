type 'a dns_result = ('a, string) result

module Id () : sig
  type id = ..

  val register_id_printer : (Format.formatter -> id -> bool) -> unit
  val pp_id : Format.formatter -> id -> unit
end = struct
  type id = ..

  let id_printers = ref []
  let register_id_printer printer = id_printers := printer :: !id_printers

  let pp_id out (id : id) =
    let rec print = function
      | [] -> Format.pp_print_string out "Unknown printer"
      | f :: fs -> if f out id then () else print fs
    in
    print !id_printers
end

module Zone = struct
  include Id ()
end

module Dns_record = struct
  include Id ()

  type t = {
    id : id;
    type_ : string;  (** The type of this record (e.g. [A], [MX])*)
    name : string;  (** The domain name of this record (e.g. [example.com]). *)
    content : string;  (** The contents of this record (e.g. [127.0.0.1])*)
    ttl : int;  (** The TTL of this record. *)
    zone_id : Zone.id;  (** The zone this record belongs to. *)
    priority : int option;  (** The priority of this record (only set for [MX] records).*)
  }

  let pp_short fmt r =
    Format.fprintf fmt "%5s %s='%s' (ttl=%d" r.type_ r.name r.content r.ttl;
    Option.iter (fun p -> Format.fprintf fmt ", priority=%d" p) r.priority;
    Format.fprintf fmt ")"

  let pp fmt r = Format.fprintf fmt "{%a} %a" pp_id r.id pp_short r
end

module type Provider = sig
  type client

  (** Find a zone. Returns [None] on failure. *)
  val find_zone : client:client -> string -> Zone.id dns_result

  (** List all records in a zone. Returns [None] on failure. *)
  val list_records : client:client -> zone:Zone.id -> Dns_record.t list dns_result

  (** Create a new record, using the same fields as those defined in {!t}. *)
  val add :
    client:client ->
    zone:Zone.id ->
    type_:string ->
    name:string ->
    content:string ->
    ?ttl:int ->
    ?priority:int ->
    unit ->
    Dns_record.id dns_result

  (** Edit an existing record, using the same fields as those defined in {!t}. *)
  val update :
    client:client ->
    zone:Zone.id ->
    type_:string ->
    name:string ->
    content:string ->
    ?ttl:int ->
    ?priority:int ->
    Dns_record.id ->
    unit dns_result

  (** Delete a record. Returns [None] if an error occurred. *)
  val delete : client:client -> zone:Zone.id -> Dns_record.id -> unit dns_result
end

type client = Client : (module Provider with type client = 'c) * 'c -> client
