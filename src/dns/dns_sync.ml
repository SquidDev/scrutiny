open Types
open Eio.Std
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

type t = {
  type_ : string;
  name : string;
  content : string;
  ttl : int option;
  priority : int option;
}

let pp fmt r =
  Format.fprintf fmt "%5s %s='%s'" r.type_ r.name r.content;
  match (r.ttl, r.priority) with
  | None, None -> ()
  | Some ttl, None -> Format.fprintf fmt " (ttl=%d)" ttl
  | None, Some priority -> Format.fprintf fmt " (priority=%d)" priority
  | Some ttl, Some priority -> Format.fprintf fmt " (ttl=%d, priority=%d)" ttl priority

let mk ?priority ~ttl ~name type_ content = { type_; name; content; ttl; priority }
let txt ?ttl ~name content = mk ~ttl ~name "TXT" content
let a ?ttl ~name content = mk ~ttl ~name "A" content
let aaaa ?ttl ~name content = mk ~ttl ~name "AAAA" content
let cname ?ttl ~name content = mk ~ttl ~name "CNAME" content
let mx ?ttl ?(priority = 10) ~name content = mk ~ttl ~name ~priority "MX" content
let caa ?ttl ~name content = Printf.sprintf "0 issue %S" content |> mk ~ttl ~name "CAA"

module Updates = struct
  let delete ~dryrun ~client ~zone record =
    let (Client ((module C), client)) = client in
    Log.warn (fun f -> f "Deleting record %a" Dns_record.pp record);
    let diff () = [ (`Remove, Format.asprintf "%a" Dns_record.pp_short record) ] in
    if dryrun then (true, diff ())
    else
      match C.delete ~client ~zone record.id with
      | Ok () -> (true, diff ())
      | Error e ->
          Log.err (fun f -> f "Error deleting record %a: %s" Dns_record.pp record e);
          (false, [ (`Same, Format.asprintf "%a" Dns_record.pp_short record) ])

  let update ~dryrun ~client ~zone spec record =
    let (Client ((module C), client)) = client in
    Log.warn (fun f -> f "Updating record %a => %a" Dns_record.pp record pp spec);
    let diff () =
      [
        (`Remove, Format.asprintf "%a" Dns_record.pp_short record);
        (`Add, Format.asprintf "%a" pp spec);
      ]
    in
    if dryrun then (true, diff ())
    else
      match
        C.update ~client ~zone ~type_:spec.type_ ~name:spec.name ~content:spec.content ?ttl:spec.ttl
          record.id
      with
      | Ok _ -> (true, diff ())
      | Error e ->
          Log.err (fun f -> f "Error updating record %a => %a: %s" Dns_record.pp record pp spec e);
          (false, [ (`Same, Format.asprintf "%a" Dns_record.pp_short record) ])

  let add ~dryrun ~client ~zone spec =
    let (Client ((module C), client)) = client in
    Log.warn (fun f -> f "Adding record %a" pp spec);
    let diff () = [ (`Add, Format.asprintf "%a" pp spec) ] in
    if dryrun then (true, diff ())
    else
      match
        C.add ~client ~zone ~type_:spec.type_ ~name:spec.name ~content:spec.content ?ttl:spec.ttl
          ?priority:spec.priority ()
      with
      | Ok id ->
          Log.info (fun f -> f "Created record %a as %a" pp spec Dns_record.pp_id id);
          (true, diff ())
      | Error e ->
          Log.err (fun f -> f "Error adding record %a: %s" pp spec e);
          (false, [])
end

module Sync = struct
  (** A pair of record kind/type and domain. *)
  module Key = struct
    type t = string * string

    let compare (kind1, domain1) (kind2, domain2) =
      match String.compare domain1 domain2 with
      | 0 -> String.compare kind1 kind2
      | n -> n
  end

  module KMap = Map.Make (Key)
  module SMap = Map.Make (String)

  (** Group domains into a mapping of (type * domain) => content => 'a, where 'a is either a [t] or
      a list of [Api_dns_record.t]. *)
  let group_domains ~key ~value ~merge domains =
    List.fold_left
      (fun map entry ->
        KMap.update (key entry)
          (fun map ->
            let map = Option.value ~default:SMap.empty map in
            let map = SMap.update (value entry) (fun xs -> Some (merge entry xs)) map in
            Some map)
          map)
      KMap.empty domains

  let rec accumulate = function
    | [] -> (true, [])
    | (ok, x) :: xs ->
        let ok', ys = accumulate xs in
        (ok && ok', x @ ys)

  let sync_domain ~client ~zone ~dryrun spec records =
    (* Find records who have matching content and a compatible priority. Those records can be
       updated in line. *)
    let overlap =
      SMap.merge
        (fun _ l r ->
          match (l, r) with
          | Some l, Some [ r ] when l.priority = r.Dns_record.priority -> Some (l, r)
          | _ -> None)
        spec records
    in

    let ok, diff =
      (* We build lists of any non-inline-updatable records. These we'll just delete and update in
         parallel. *)
      let spec =
        SMap.to_seq spec
        |> Seq.filter (fun (k, _) -> not (SMap.mem k overlap))
        |> Seq.map snd |> List.of_seq
      in
      let records =
        SMap.to_seq records
        |> Seq.filter (fun (k, _) -> not (SMap.mem k overlap))
        |> Seq.map snd |> Seq.flat_map List.to_seq |> List.of_seq
      in
      match (spec, records) with
      | [ spec ], [ record ] when record.Dns_record.priority = spec.priority ->
          (* If we've got exactly one record compatible with different content then we'll update it
             instead of deleting/updating. This just makes the diff look nicer. *)
          Updates.update ~dryrun ~client ~zone spec record
      | _, _ -> (
          (* Otherwise there's no point trying to find the minimal set of updates. Just clobber
             everything. We bulk-delete and then bulk-insert, just to avoid any potential race
             conditions. *)
          let result = Fiber.map (Updates.delete ~dryrun ~client ~zone) records in
          match accumulate result with
          | false, result -> (false, result)
          | true, result ->
              let result' = Fiber.map (Updates.add ~dryrun ~client ~zone) spec in
              let ok, result' = accumulate result' in
              (ok, result @ result'))
    and ok', overlaps =
      (* In parallel to the above, perform in-sequence updates. *)
      SMap.to_seq overlap |> List.of_seq
      |> Fiber.map (fun (_, (spec, record)) ->
             if Option.fold ~none:true ~some:(fun ttl -> ttl = record.Dns_record.ttl) spec.ttl then (
               Log.debug (fun f -> f "Nothing to do for record %a" Dns_record.pp record);
               (true, [ (`Same, Format.asprintf "%a" pp spec) ]))
             else Updates.update ~dryrun ~client ~zone spec record)
      |> accumulate
    in
    (ok && ok', diff @ overlaps)

  exception Duplicate of t

  let sync ?(dryrun = false) ~client:client_ ~zone spec =
    let (Client ((module C), client)) = client_ in
    match
      (* We require the invariant that there can only be one entry for any (kind, domain, content)
         pair. We allow multiple remote ones, but will remove all but the first. *)
      group_domains
        ~key:(fun x -> (x.type_, x.name))
        ~value:(fun x -> x.content)
        ~merge:(fun entry xs -> if Option.is_some xs then raise (Duplicate entry) else entry)
        spec
    with
    | exception Duplicate e ->
        (Error (Format.asprintf "Duplicate record %a" pp e), Scrutiny_diff.empty)
    | spec -> (
      match C.list_records ~client ~zone with
      | Error e ->
          Log.err (fun f -> f "Error listing records: %s" e);
          (Error "Cannot list DNS records", Scrutiny_diff.empty)
      | Ok records ->
          let records =
            group_domains
              ~key:(fun x -> (x.Dns_record.type_, x.name))
              ~value:(fun x -> x.content)
              ~merge:(fun entry xs -> entry :: Option.value ~default:[] xs)
              records
          in
          let xs =
            KMap.merge
              (fun _ spec records ->
                let spec = Option.value ~default:SMap.empty spec in
                let records = Option.value ~default:SMap.empty records in
                Some (spec, records))
              spec records
            |> KMap.to_seq |> List.of_seq
            |> Fiber.map (fun (_, (spec, records)) ->
                   sync_domain ~dryrun ~client:client_ ~zone spec records)
          in
          let ok = List.for_all fst xs in
          let diff = List.map snd xs |> List.flatten |> Scrutiny_diff.of_lines in
          ((if ok then Ok () else Error "Some requests failed, see log"), diff))
end

let sync = Sync.sync
