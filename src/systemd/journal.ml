open Ctypes
module Constants = Systemd_bindings.Constants (Ffi_generated_types)
module Values = Systemd_bindings.Values (Ffi_generated)

let unix_err ~msg x =
  let error = Scrutiny_errno.convert x in
  let error_msg = Unix.error_message error in
  raise (Unix.Unix_error (error, error_msg, msg))

let check ~msg x = if x >= 0 then x else unix_err ~msg (-x)
let check_ ~msg x = if x >= 0 then () else unix_err ~msg (-x)

module C = Constants.Journal
module V = Values.Journal

type t = {
  journal : V.sd_journal;
  data : unit ptr ptr;
  data_size : Unsigned.Size_t.t ptr;
  mutable fd : Lwt_unix.file_descr option;
}

type flags = LocalOnly

let int_of_flag = function
  | LocalOnly -> C.sd_journal_local_only

let open_ ?(flags = [ LocalOnly ]) () =
  let flags = List.fold_left (fun x y -> x lor int_of_flag y) 0 flags in
  let journal = allocate_n ~count:1 V.sd_journal in
  V.sd_journal_open journal flags |> check_ ~msg:"sd_journal_open";

  let data = allocate_n ~count:1 (ptr void) in
  let data_size = allocate_n ~count:1 size_t in
  { journal = !@journal; data; data_size; fd = None }

let close { journal; _ } = V.sd_journal_close journal
let no_timeout = Unsigned.UInt64.of_int (-1)

type journal_change =
  | Nop
  | Append
  | Invalidate
  | Unknown of int

type level =
  | Alert
  | Crit
  | Error
  | Warning
  | Notice
  | Info
  | Debug

let convert_wait ret =
  if ret = C.sd_journal_nop then Nop
  else if ret = C.sd_journal_append then Append
  else if ret = C.sd_journal_invalidate then Invalidate
  else Unknown ret

let wait ?(timeout = no_timeout) { journal; _ } =
  V.sd_journal_wait journal timeout |> check ~msg:"sd_journal_wait" |> convert_wait

let wait_async journal =
  let fd =
    match journal.fd with
    | Some fd -> fd
    | None ->
        let fd = V.sd_journal_get_fd journal.journal |> check ~msg:"sd_journal_get_fd" in
        let fd : Unix.file_descr = Obj.magic fd (* Dodgy but safe*) in
        let fd = Lwt_unix.of_unix_file_descr fd in
        journal.fd <- Some fd;
        fd
  in
  let open Lwt.Infix in
  Lwt_unix.wait_read fd >|= fun () ->
  V.sd_journal_process journal.journal |> check ~msg:"sd_journal_process" |> convert_wait

let next { journal; _ } =
  let ret = V.sd_journal_next journal |> check ~msg:"sd_journal_next" in
  ret <> 0

let seek_tail { journal; _ } = V.sd_journal_seek_tail journal |> check_ ~msg:"sd_journal_seek_tail"

let get_data_raw_exn { journal; data; data_size; _ } field =
  V.sd_journal_get_data journal field data data_size |> check_ ~msg:"sd_journal_get_data";
  let ptr = !@data |> from_voidp char in
  let offset = String.length field + 1 in
  let length = Unsigned.Size_t.to_int !@data_size - offset in
  (ptr +@ offset, length)

let get_data_exn j field =
  let ptr, length = get_data_raw_exn j field in
  string_from_ptr ~length ptr

let get_data_raw { journal; data; data_size; _ } field =
  let ret = V.sd_journal_get_data journal field data data_size in
  if ret >= 0 then
    let ptr = !@data |> from_voidp char in
    let offset = String.length field + 1 in
    let length = Unsigned.Size_t.to_int !@data_size - offset in
    Some (ptr +@ offset, length)
  else
    match Scrutiny_errno.convert (-ret) with
    | Unix.ENOENT -> None
    | error -> raise (Unix.Unix_error (error, Unix.error_message error, "sd_journal_get_data"))

let get_data j field =
  get_data_raw j field |> Option.map @@ fun (ptr, length) -> string_from_ptr ~length ptr

let level_strings =
  let open Constants.Syslog in
  [
    (log_alert, Alert);
    (log_crit, Crit);
    (log_err, Error);
    (log_warning, Warning);
    (log_notice, Notice);
    (log_info, Info);
    (log_debug, Debug);
  ]
  |> List.to_seq
  |> Seq.map (fun (k, v) -> (string_of_int k, v))
  |> Hashtbl.of_seq

let get_level j : level =
  match get_data j "PRIORITY" with
  | None -> Error
  | Some x -> Hashtbl.find_opt level_strings x |> Option.value ~default:(Error : level)

module Write = struct
  let write_ msgs =
    let count = List.length msgs in
    let items = Array.make count null in
    let msg_buffer = allocate_n ~count Constants.Readv.iovec in
    List.iteri
      (fun i msg ->
        let entry = !@(msg_buffer +@ i) in
        let ptr = CArray.(of_string msg |> start) |> to_voidp in
        (* No clue if needed, but make sure to keep this around in memory. *)
        items.(i) <- ptr;
        setf entry Constants.Readv.iov_base ptr;
        setf entry Constants.Readv.iov_len (String.length msg |> Unsigned.Size_t.of_int))
      msgs;
    V.sd_journal_sendv msg_buffer count |> check_ ~msg:"sd_journal_sendv";

    Sys.opaque_identity items |> ignore

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
    | Alert -> "PRIORITY=" ^ string_of_int Constants.Syslog.log_alert
    | Crit -> "PRIORITY=" ^ string_of_int Constants.Syslog.log_crit
    | Error -> "PRIORITY=" ^ string_of_int Constants.Syslog.log_err
    | Warning -> "PRIORITY=" ^ string_of_int Constants.Syslog.log_warning
    | Notice -> "PRIORITY=" ^ string_of_int Constants.Syslog.log_notice
    | Info -> "PRIORITY=" ^ string_of_int Constants.Syslog.log_info
    | Debug -> "PRIORITY=" ^ string_of_int Constants.Syslog.log_debug

  let write ?(tag = def_tag) ?(level : level = Error) ?(extra = []) message =
    write_
    @@ map_priority level :: ("SYSLOG_IDENTIFIER=" ^ tag) :: ("MESSAGE=" ^ message)
       :: prepare_extra extra
end
