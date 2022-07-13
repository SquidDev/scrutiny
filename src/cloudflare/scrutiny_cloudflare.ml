type auth = Request.auth =
  | Token of string
  | Email of string * string

module Client = struct
  type t = Request.client

  let with_client = Request.with_client
end

module type Id = sig
  type id

  val pp_id : Format.formatter -> id -> unit
end

module Zone = Api_zone

module DnsRecord = struct
  include Api_dns_record
  module Spec = Dns_sync
end
