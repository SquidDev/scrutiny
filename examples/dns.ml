open Eio.Std
open Scrutiny_dns

let source : source =
  Porkbun { api_key = Sys.getenv "PORKBUN_API_KEY"; secret = Sys.getenv "PORKBUN_API_SECRET" }

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

  exit @@ Eio_main.run
  @@ fun env ->
  Switch.run @@ fun sw ->
  let client = Client.create ~sw ~clock:env#clock ~net:env#net source in
  let ok, result =
    match Zone.find ~client site with
    | Ok zone -> DnsRecord.Spec.sync ~dryrun:true ~client ~zone records
    | Error e -> (Error (Format.asprintf "Cannot find %s: %s" site e), Scrutiny_diff.empty)
  in
  Format.printf "%a@." (Scrutiny_diff.pp ~full:false) result;
  match ok with
  | Ok () -> 0
  | Error e -> print_endline e; 1
