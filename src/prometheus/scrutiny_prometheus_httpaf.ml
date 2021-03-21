open Httpaf

let handle_error request status =
  let headers = Headers.of_list [ ("connection", "close") ] in
  Reqd.respond_with_string request (Response.create ~headers status) ""

let handle snapshot request = function
  | { Request.meth = `GET; _ } ->
      let response =
        Response.create
          ~headers:
            (Headers.of_list
               [ ("content-type", "text/plain; version=0.0.4"); ("connection", "close") ])
          `OK
      in

      (* We use our own buffer to avoid allocating one massive string. This is probably a little
         overkill. *)
      let response_body = Reqd.respond_with_streaming request response in
      let formatter =
        Format.make_formatter
          (fun buffer off len -> Body.write_string response_body buffer ~off ~len)
          (fun () -> ())
      in
      Scrutiny_prometheus.TextFormat_0_0_4.output formatter snapshot;
      Format.pp_print_flush formatter ();
      Body.close_writer response_body
  | _ -> handle_error request `Method_not_allowed

let handle_default reqd request = handle Prometheus.CollectorRegistry.(collect default) reqd request
