(** A terrible cgroups implementation. *)

module Log = (val Logs.src_log (Logs.Src.create "scrutiny.cgroups"))

type kind =
  | Unified
  | V2

type t = {
  kind : kind;
  path : Fpath.t;
}

let get path =
  if Sys.file_exists Fpath.(path / "unified" |> to_string) then Ok { kind = Unified; path }
  else if Sys.file_exists Fpath.(path / "cgroup.controllers" |> to_string) then
    Ok { kind = V2; path }
  else Format.asprintf "Unknown Cgroups hierarchy at %a" Fpath.pp path |> Result.error

let get_path ~subsystem ~stat group t =
  if Fpath.is_abs group then invalid_arg "Path cannot be absolute";
  match t.kind with
  | Unified -> Fpath.(t.path / subsystem // group / stat)
  | V2 -> Fpath.(t.path // group / stat)

let use ~fs ~subsystem ~stat cgroup t f =
  let path = get_path ~subsystem ~stat cgroup t in
  try Eio.Path.(fs / Fpath.to_string path) |> Eio.Path.load |> f
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
    Log.warn (fun f -> f "Cannot find stat %s for cgroup %a" stat Fpath.pp cgroup);
    None

(** [prefix_at ~start ~pre str]: Does [str] contain the string [pre] at position [start]?
    Effectively {!CCString.prefix} with an offset. *)
let prefix_at ~start ~pre s =
  let len = String.length pre in
  if len > String.length s - start then false
  else
    let rec check i =
      if i = len then true
      else if Stdlib.( <> ) (String.unsafe_get s (i + start)) (String.unsafe_get pre i) then false
      else check (i + 1)
    in
    check 0

(** Find a file within a cgroups2 statistics file. *)
let find_field ~field contents =
  let field = field ^ " " in
  let rec go start =
    if prefix_at ~start ~pre:field contents then
      let end_ = String.index_from contents start '\n' in
      let start = start + String.length field in
      String.sub contents start (end_ - start) |> Option.some
    else
      match String.index_from_opt contents start '\n' with
      | None ->
          Log.warn (fun f -> f "%S" contents);
          None
      | Some x -> go (x + 1)
  in
  go 0

let get_memory_current ~fs cgroup t =
  let stat =
    match t.kind with
    | Unified -> "memory.usage_in_bytes"
    | V2 -> "memory.current"
  in
  use ~fs ~subsystem:"memory" ~stat cgroup t @@ fun x ->
  match String.trim x |> int_of_string_opt with
  | None ->
      Log.warn (fun f -> f "Error parsing memory usage for %a (contents is %S)" Fpath.pp cgroup x);
      None
  | Some _ as x -> x

let get_memory_anon ~fs cgroup t =
  use ~fs ~subsystem:"unified" ~stat:"memory.stat" cgroup t @@ fun x ->
  match find_field ~field:"anon" x |> CCOption.flat_map int_of_string_opt with
  | None ->
      Log.warn (fun f -> f "Error parsing memory usage for %a (contents is %S)" Fpath.pp cgroup x);
      None
  | Some _ as x -> x

let get_cpu ~fs cgroup t =
  use ~fs ~subsystem:"unified" ~stat:"cpu.stat" cgroup t @@ fun x ->
  match find_field ~field:"usage_usec" x |> CCOption.flat_map int_of_string_opt with
  | None ->
      Log.warn (fun f -> f "Error parsing cpu usage for %a (contents is %S)" Fpath.pp cgroup x);
      None
  | Some _ as x -> x
