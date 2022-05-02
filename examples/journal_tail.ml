(** A basic module to tail the journal and print log messages. *)

module Journal = Scrutiny_systemd.Journal

let () =
  let journal = Journal.open_ () in
  Journal.seek_tail journal;
  while true do
    if Journal.next journal then
      let message = Journal.get_data_exn journal "MESSAGE"
      and unit = Journal.get_data journal "_SYSTEMD_UNIT" |> Option.value ~default:"(no unit)"
      and tag = Journal.get_data journal "SYSLOG_IDENTIFIER" |> Option.value ~default:"(no tag)"
      and tz =
        let t = Unix.gmtime (Journal.get_realtime journal) in
        Printf.sprintf "%04d-%02d-%02d %02d:%02d:%0d" (1900 + t.tm_year) (t.tm_mon + 1) t.tm_mday
          t.tm_hour t.tm_min t.tm_sec
      in
      Printf.printf "%s [%s/%s] %s\n%!" tz unit tag message
    else Journal.wait journal |> ignore
  done
