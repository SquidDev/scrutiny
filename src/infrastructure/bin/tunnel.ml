open struct
  (* Needed to ensure these aren't removed from the binary. *)

  let _ = Scrutiny_infrastructure_resources.File_resource.file

  let _ = Scrutiny_infrastructure_resources.Service_resource.service
end

let () =
  Printexc.record_backtrace true;
  let open Cmdliner in
  let tunnel_cmd =
    let doc = "Starts a tunnel reading commands from stdin. Should not be run manually" in
    let term () = Lwt_main.run (Scrutiny_infrastructure.Executor.run_tunnel ()) in
    (Term.(const term $ const ()), Term.info "scrutiny-infra-tunnel" ~doc)
  in

  Term.exit @@ Term.eval tunnel_cmd
