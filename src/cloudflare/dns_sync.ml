open Lwt.Infix
open Lwt.Syntax

module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

type t =
  { type_ : string;
    name : string;
    content : string;
    proxied : bool;
    ttl : int;
    priority : int option;
    data : Yojson.Safe.t option
  }

let pp_json fmt = function
  | None -> ()
  | Some x -> Format.fprintf fmt " %s" (Yojson.Safe.to_string x)

let pp fmt r = Format.fprintf fmt "%5s %s=%S%a" r.type_ r.name r.content pp_json r.data

let mk ?priority ?(proxied = false) ~ttl ~name ?data type_ content =
  { type_; name; content; proxied; ttl; priority; data }

let txt ?(ttl = 1) ~name content = mk ~ttl ~name "TXT" content

let a ?(ttl = 1) ?(proxied = false) ~name content = mk ~ttl ~proxied ~name "A" content

let aaaa ?(ttl = 1) ?(proxied = false) ~name content = mk ~ttl ~proxied ~name "AAAA" content

let cname ?(ttl = 1) ?(proxied = false) ~name content = mk ~ttl ~proxied ~name "CNAME" content

let mx ?(ttl = 1) ?(priority = 10) ~name content = mk ~ttl ~name ~priority "MX" content

let caa ?(ttl = 1) ~name content =
  let data = `Assoc [ ("flags", `Int 0); ("tag", `String "issue"); ("value", `String content) ] in
  Printf.sprintf "0 issue %S" content |> mk ~ttl ~name ~data "CAA"

module Updates = struct
  let delete ~dryrun ~auth ~zone record =
    Log.warn (fun f -> f "Deleting record %a" Api_dns_record.pp record);
    let diff () = [ (`Remove, Format.asprintf "%a" Api_dns_record.pp_short record) ] in
    if dryrun then Lwt.return (true, diff ())
    else
      Api_dns_record.delete ~auth ~zone record.id >|= function
      | Some () -> (true, diff ())
      | None ->
          Log.err (fun f -> f "Error deleting record %a" Api_dns_record.pp record);
          (false, [ (`Same, Format.asprintf "%a" Api_dns_record.pp_short record) ])

  let update ~dryrun ~auth ~zone spec record =
    Log.warn (fun f -> f "Updating record %a => %a" Api_dns_record.pp record pp spec);
    let diff () =
      [ (`Remove, Format.asprintf "%a" Api_dns_record.pp_short record);
        (`Add, Format.asprintf "%a" pp spec)
      ]
    in
    if dryrun then Lwt.return (true, diff ())
    else
      Api_dns_record.update ~auth ~zone ~type_:spec.type_ ~name:spec.name ~content:spec.content
        ~proxied:spec.proxied ~ttl:spec.ttl record.id
      >|= function
      | Some _ -> (true, diff ())
      | None ->
          Log.err (fun f -> f "Error updating record %a => %a" Api_dns_record.pp record pp spec);
          (false, [ (`Same, Format.asprintf "%a" Api_dns_record.pp_short record) ])

  let add ~dryrun ~auth ~zone spec =
    Log.warn (fun f -> f "Adding record %a" pp spec);
    let diff () = [ (`Add, Format.asprintf "%a" pp spec) ] in
    if dryrun then Lwt.return (true, diff ())
    else
      Api_dns_record.add ~auth ~zone ~type_:spec.type_ ~name:spec.name ~content:spec.content
        ~proxied:spec.proxied ~ttl:spec.ttl ?priority:spec.priority ?data:spec.data ()
      >|= function
      | Some record ->
          Log.info (fun f -> f "Created record %a as %s" pp spec record.id);
          (true, diff ())
      | None ->
          Log.err (fun f -> f "Error adding record %a" pp spec);
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

  let sync_domain ~auth ~zone ~dryrun spec records =
    (* Find records who have matching content and a compatible priority (the API doesn't allow us to
       change that). Those records can be updated in line. *)
    let overlap =
      SMap.merge
        (fun _ l r ->
          match (l, r) with
          | Some l, Some [ r ]
            when l.priority = r.Api_dns_record.priority && l.data = r.Api_dns_record.data ->
              Some (l, r)
          | _ -> None)
        spec records
    in

    let+ ok, diff =
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
      | [ spec ], [ record ] when record.Api_dns_record.priority = spec.priority ->
          (* If we've got exactly one record compatible with different content then we'll update it
             instead of deleting/updating. This just makes the diff look nicer. *)
          Updates.update ~dryrun ~auth ~zone spec record
      | _, _ -> (
          (* Otherwise there's no point trying to find the minimal set of updates. Just clobber
             everything. We bulk-delete and then bulk-insert, just to avoid any potential race
             conditions. *)
          let* result = Lwt_list.map_p (Updates.delete ~dryrun ~auth ~zone) records in
          match accumulate result with
          | false, result -> Lwt.return (false, result)
          | true, result ->
              let+ result' = Lwt_list.map_p (Updates.add ~dryrun ~auth ~zone) spec in
              let ok, result' = accumulate result' in
              (ok, result @ result'))
    and+ ok', overlaps =
      (* In parallel to the above, perform in-sequence updates. *)
      SMap.to_seq overlap |> List.of_seq
      |> Lwt_list.map_p (fun (_, (spec, record)) ->
             if spec.ttl = record.Api_dns_record.ttl && spec.proxied = record.proxied then (
               Log.info (fun f -> f "Nothing to do for record %a" Api_dns_record.pp record);
               Lwt.return (true, [ (`Same, Format.asprintf "%a" pp spec) ]))
             else Updates.update ~dryrun ~auth ~zone spec record)
      >|= accumulate
    in
    (ok && ok', diff @ overlaps)

  exception Duplicate of t

  let sync ?(dryrun = false) ~auth ~zone spec =
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
        Lwt.return (Error (Format.asprintf "Duplicate record %a" pp e), Scrutiny_diff.empty)
    | spec -> (
        Api_dns_record.list ~auth ~zone >>= function
        | None -> Lwt.return (Error "Cannot list features", Scrutiny_diff.empty)
        | Some records ->
            let records =
              group_domains
                ~key:(fun x -> (x.Api_dns_record.type_, x.name))
                ~value:(fun x -> x.content)
                ~merge:(fun entry xs -> entry :: Option.value ~default:[] xs)
                records
            in
            KMap.merge
              (fun _ spec records ->
                let spec = Option.value ~default:SMap.empty spec in
                let records = Option.value ~default:SMap.empty records in
                Some (spec, records))
              spec records
            |> KMap.to_seq |> List.of_seq
            |> Lwt_list.map_p (fun (_, (spec, records)) ->
                   sync_domain ~dryrun ~auth ~zone spec records)
            >|= fun xs ->
            let ok = List.for_all fst xs in
            let diff = List.map snd xs |> List.flatten |> Scrutiny_diff.of_lines in
            ((if ok then Ok () else Error "Some requests failed, see log"), diff))
end

let sync = Sync.sync
