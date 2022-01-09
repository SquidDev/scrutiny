module Infra = Scrutiny_infrastructure
module CF = Scrutiny_cloudflare

module State = struct
  type t = {
    spec : CF.DnsRecord.Spec.t list;
    auth : CF.auth;
  }

  let yojson_of_t x : Yojson.Safe.t = `String (Marshal.to_string x [])

  let t_of_yojson : Yojson.Safe.t -> t = function
    | `String x -> Marshal.from_string x 0
    | _ -> raise (Yojson.Json_error "Malformed Scrutiny_diff")

  let digest { spec; _ } = Marshal.to_string spec [] |> Digest.string |> Digest.to_hex
end

module CFResource = struct
  let id = "cloudflare"

  module Key = Infra.String
  module EdgeOptions = Infra.Unit
  module Value = State

  let pp out x = Fmt.fmt "Zone %s" out x

  let apply ~zone ~auth ~spec () =
    match%lwt CF.DnsRecord.Spec.sync ~dryrun:false ~auth ~zone spec with
    | Error e, _ -> Lwt.return_error e
    | Ok (), _ -> Lwt.return_ok ()

  let apply zone ({ auth; spec } : State.t) () : (Infra.change, string) result Lwt.t =
    match%lwt CF.Zone.find ~auth zone with
    | None -> Lwt.return_error "Cannot find zone"
    | Some zone -> (
        match%lwt CF.DnsRecord.Spec.sync ~dryrun:true ~auth ~zone spec with
        | Error e, _ -> Lwt.return_error e
        | Ok (), diff ->
            let apply : Infra.change =
              if Scrutiny_diff.is_empty diff then Correct
              else NeedsChange { diff; apply = apply ~zone ~auth ~spec }
            in
            Lwt.return_ok apply)
end

let cf_module =
  Infra.Resource.make
    (module CFResource : Infra.Resource
      with type Key.t = string
       and type EdgeOptions.t = unit
       and type Value.t = State.t)

let zone id ~auth spec =
  Infra.Rules.resource cf_module id @@ fun () ->
  let open Infra.Action in
  let+ spec = spec () in
  Lwt.return { State.auth; spec }
