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

  let apply ~env ~zone ~source ~spec () =
    Lwt_eio.run_eio @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let client = Dns.Client.create ~sw ~clock:env#clock ~net:env#net source in

    let res, _diff = Dns.DnsRecord.Spec.sync ~dryrun:false ~client ~zone spec in
    res

  let apply ~env zone ({ source; spec } : State.t) () : (Infra.change, string) result Lwt.t =
    Lwt_eio.run_eio @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let client = Dns.Client.create ~sw ~clock:env#clock ~net:env#net source in

    match Dns.Zone.find ~client zone with
    | Error e -> Error ("Cannot find zone: " ^ e)
    | Ok zone -> (
      match Dns.DnsRecord.Spec.sync ~dryrun:true ~client ~zone spec with
      | Error e, _ -> Error e
      | Ok (), diff ->
          let apply : Infra.change =
            if Scrutiny_diff.is_empty diff then Correct
            else NeedsChange { diff; apply = apply ~env ~zone ~source ~spec }
          in
          Ok apply)
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
