module Native = struct
  type sd_journal

  external sd_journal_open : int -> sd_journal = "scrutiny_sd_journal_open"
  external sd_journal_close : sd_journal -> unit = "scrutiny_sd_journal_close"
  external sd_journal_get_fd : sd_journal -> Unix.file_descr = "scrutiny_sd_journal_get_fd"
  external sd_journal_process : sd_journal -> int = "scrutiny_sd_journal_process"
  external sd_journal_next : sd_journal -> bool = "scrutiny_sd_journal_next"
  external sd_journal_seek_tail : sd_journal -> unit = "scrutiny_sd_journal_seek_tail"

  external sd_journal_get_data : sd_journal -> string -> string option
    = "scrutiny_sd_journal_get_data"

  external sd_journal_get_realtime_usec : sd_journal -> float
    = "scrutiny_sd_journal_get_realtime_usec"

  external sd_journal_sendv_array : string array -> unit = "scrutiny_sd_journal_sendv_array"
  external sd_journal_sendv_list : string list -> unit = "scrutiny_sd_journal_sendv_list"
end

type t = {
  journal : Native.sd_journal;
  mutable is_open : bool;
  mutable fd : Lwt_unix.file_descr option;
}

type flags = LocalOnly

let int_of_flag = function
  | LocalOnly -> 1 lsl 0

let close x =
  if not x.is_open then failwith "close: Already closed";
  x.is_open <- false;
  x.fd <- None;
  Native.sd_journal_close x.journal

let open_ ?sw ?(flags = [ LocalOnly ]) () =
  Lwt_switch.check sw;
  let flags = List.fold_left (fun x y -> x lor int_of_flag y) 0 flags in
  let journal = Native.sd_journal_open flags in
  let journal = { journal; is_open = true; fd = None } in
  Lwt_switch.add_hook sw (fun () -> close journal; Lwt.return_unit);
  journal

type journal_change =
  | Nop
  | Append
  | Invalidate
  | Unknown of int

type level =
  | Emerg
  | Alert
  | Crit
  | Error
  | Warning
  | Notice
  | Info
  | Debug

let convert_wait = function
  | 0 -> Nop
  | 1 -> Append
  | 2 -> Invalidate
  | x -> Unknown x

let wait journal =
  let fd =
    match journal.fd with
    | Some fd -> fd
    | None ->
        let fd =
          Native.sd_journal_get_fd journal.journal |> Lwt_unix.of_unix_file_descr ~blocking:false
        in
        journal.fd <- Some fd;
        fd
  in
  let open Lwt.Infix in
  Lwt_unix.wait_read fd >|= fun () -> Native.sd_journal_process journal.journal |> convert_wait

let next { journal; _ } = Native.sd_journal_next journal
let seek_tail { journal; _ } = Native.sd_journal_seek_tail journal
let get_data_exn { journal; _ } field = Native.sd_journal_get_data journal field |> Option.get
let get_data { journal; _ } field = Native.sd_journal_get_data journal field
let get_realtime { journal; _ } = Native.sd_journal_get_realtime_usec journal

let level_strings =
  [
    (0, Emerg); (1, Alert); (2, Crit); (3, Error); (4, Warning); (5, Notice); (6, Info); (7, Debug);
  ]
  |> List.to_seq
  |> Seq.map (fun (k, v) -> (string_of_int k, v))
  |> Hashtbl.of_seq

let get_level j : level =
  match get_data j "PRIORITY" with
  | None -> Error
  | Some x -> Hashtbl.find_opt level_strings x |> Option.value ~default:(Error : level)

module Write = struct
  let write_fields = Native.sd_journal_sendv_list
  let write_fields_array = Native.sd_journal_sendv_array

  let prepare_extra = function
    | [] -> []
    | extra ->
        let buf = Buffer.create 64 in
        List.map
          (fun (k, v) ->
            Buffer.clear buf;
            Buffer.add_string buf k;
            Buffer.add_char buf '=';
            Buffer.add_string buf v;
            Buffer.contents buf)
          extra

  let def_tag = if Array.length Sys.argv = 0 then Sys.executable_name else Sys.argv.(0)

  let map_priority = function
    | Emerg -> "PRIORITY=0"
    | Alert -> "PRIORITY=1"
    | Crit -> "PRIORITY=2"
    | Error -> "PRIORITY=3"
    | Warning -> "PRIORITY=4"
    | Notice -> "PRIORITY=5"
    | Info -> "PRIORITY=6"
    | Debug -> "PRIORITY=7"

  let write ?(tag = def_tag) ?(level : level = Error) ?(extra = []) message =
    write_fields
    @@ map_priority level :: ("SYSLOG_IDENTIFIER=" ^ tag) :: ("MESSAGE=" ^ message)
       :: prepare_extra extra
end
