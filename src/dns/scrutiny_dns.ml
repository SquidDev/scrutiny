open Types

type source =
  | Porkbun of {
      api_key : string;
      secret : string;
    }

module Client = struct
  type t = client

  let with_client source fn =
    match source with
    | Porkbun { api_key; secret } ->
        Request.with_client (fun client ->
            let client = Porkbun.create ~client ~auth:{ api_key; secret } in
            let client = Client ((module Porkbun), client) in
            fn client)
end

module type Id = sig
  type id

  val pp_id : Format.formatter -> id -> unit
end

module Zone = struct
  include Zone

  let find ~client =
    let (Types.Client ((module C), client)) = client in
    C.find_zone ~client
end

module DnsRecord = struct
  include Dns_record

  let list ~client =
    let (Types.Client ((module C), client)) = client in
    C.list_records ~client

  let add ~client =
    let (Types.Client ((module C), client)) = client in
    C.add ~client

  let update ~client =
    let (Types.Client ((module C), client)) = client in
    C.update ~client

  let delete ~client =
    let (Types.Client ((module C), client)) = client in
    C.delete ~client

  module Spec = Dns_sync
end
