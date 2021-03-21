(** A basic module to tail the journal and print log messages. *)

module Journal = Scrutiny_systemd.Journal

let () =
  let journal = Journal.open_ () in
  Journal.seek_tail journal;
  while true do
    if Journal.next journal then Journal.get_data_exn journal "MESSAGE" |> print_endline
    else Journal.wait journal |> ignore
  done
