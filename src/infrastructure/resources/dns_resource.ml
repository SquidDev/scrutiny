module Infra = Scrutiny_infrastructure
module Dns = Scrutiny_dns

module State = struct
  type t = {
    spec : Dns.DnsRecord.Spec.t list;
    source : Dns.source;
  }

  let yojson_of_t x : Yojson.Safe.t = `String (Marshal.to_string x [])

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `String x -> Marshal.from_string x 0
    | _ -> raise (Yojson.Json_error "Malformed Scrutiny_diff")

  let digest { spec; _ } = Marshal.to_string spec [] |> Digest.string |> Digest.to_hex
end

module DnsResource = struct
  let id = "dns"

  module Key = Infra.String
  module EdgeOptions = Infra.Unit
  module Value = State

  let pp out x = Fmt.fmt "Zone %s" out x

  let apply ~zone ~source ~spec () =
    match%lwt
      Dns.Client.with_client source (fun client ->
          Dns.DnsRecord.Spec.sync ~dryrun:false ~client ~zone spec)
    with
    | Error e, _ -> Lwt.return_error e
    | Ok (), _ -> Lwt.return_ok ()

  let apply ~env:_ zone ({ source; spec } : State.t) () : (Infra.change, string) result Lwt.t =
    Dns.Client.with_client source @@ fun client ->
    match%lwt Dns.Zone.find ~client zone with
    | Error e -> Lwt.return_error ("Cannot find zone: " ^ e)
    | Ok zone -> (
        match%lwt Dns.DnsRecord.Spec.sync ~dryrun:true ~client ~zone spec with
        | Error e, _ -> Lwt.return_error e
        | Ok (), diff ->
            let apply : Infra.change =
              if Scrutiny_diff.is_empty diff then Correct
              else NeedsChange { diff; apply = apply ~zone ~source ~spec }
            in
            Lwt.return_ok apply)
end

let cf_module =
  Infra.Resource.make
    (module DnsResource : Infra.Resource
      with type Key.t = string
       and type EdgeOptions.t = unit
       and type Value.t = State.t)

let zone id ~source spec =
  Infra.Rules.resource cf_module id @@ fun () ->
  let open Infra.Action in
  let+ spec = spec () in
  Lwt.return { State.source; spec }
