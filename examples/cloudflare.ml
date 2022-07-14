open Scrutiny_cloudflare

let auth : auth =
  let key = Sys.getenv "CF_API_KEY" in
  match Sys.getenv_opt "CF_API_EMAIL" with
  | Some email -> Email (email, key)
  | None -> Token key

let site, records =
  let open DnsRecord.Spec in
  ( "github.com",
    [
      a ~name:"github.com" "140.82.121.3";
      mx ~priority:10 ~name:"github.com" "alt4.aspmx.l.google.com";
    ] )

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level ~all:true (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  exit @@ Lwt_main.run
  @@
  let open Lwt.Syntax in
  Client.with_client auth @@ fun client ->
  let* task = Zone.find ~client site in
  let+ ok, result =
    match task with
    | Some zone -> DnsRecord.Spec.sync ~dryrun:true ~client ~zone records
    | None -> Lwt.return (Error (Format.asprintf "Cannot find %s" site), Scrutiny_diff.empty)
  in
  Format.printf "%a@." (Scrutiny_diff.pp ~full:false) result;
  match ok with
  | Ok () -> 0
  | Error e -> print_endline e; 1
