open Scrutiny_infrastructure
open Scrutiny_infrastructure_resources.File_resource
open Scrutiny_infrastructure_resources.Service_resource
open Action
open Rules

let templates = Fpath.(parent (v __FILE__))

let rules =
  (* Upload a config file to /etc/nginx/nginx.conf, based on the nginx.conf template *)
  let* conf =
    template (Fpath.v "/etc/nginx/nginx.conf") ~template:Fpath.(templates / "nginx.conf")
    @@ fun () -> value []
  in
  let* unit_file =
    template
      (Fpath.v "/etc/systemd/system/openresty.service")
      ~template:Fpath.(templates / "openresty.service")
    @@ fun () -> value []
  in
  (* Ensure the "openresty" service is enabled and running. This only runs once our config file and
     systemd service have been updated. If either have changed, the openresty service will be
     restarted or reloaded as appropriate. *)
  let* _service =
    service ~name:"openresty" ~scope:`System @@ fun () ->
    let+ () = need ~options:`Reload conf and+ () = need ~options:`Restart unit_file in
    service_state ~enabled:true ~running:true ~monitor:5 ()
  in
  pure ()

let () =
  Printexc.record_backtrace true;
  Fmt_tty.setup_std_outputs ();
  Logs.set_level ~all:true (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());

  let executor = Executor.ssh ~sudo_pw:"password" ~host:"test-server" () in
  let _executor = Executor.local (* Alternative: use localhost: *) in
  main ~executor rules
