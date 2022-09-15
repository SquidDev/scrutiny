open Import

let logs_reporter =
  let pp_header out style h src =
    let time = Unix.time () |> Unix.gmtime in
    Fmt.pf out " %02d:%02d:%02d [%a/%s] " time.tm_hour time.tm_min time.tm_sec
      Fmt.(styled style string)
      h (Logs.Src.name src)
  in
  let pp_header out (l, h, s) =
    match l with
    | Logs.App -> (
      match h with
      | None -> ()
      | Some h -> pp_header out `Cyan h s)
    | Logs.Error -> pp_header out `Red (Option.value ~default:"ERROR" h) s
    | Logs.Warning -> pp_header out `Yellow (Option.value ~default:"WARNING" h) s
    | Logs.Info -> pp_header out `Blue (Option.value ~default:"INFO" h) s
    | Logs.Debug -> pp_header out `Green (Option.value ~default:"DEBUG" h) s
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags:_ fmt ->
    Format.kfprintf k Format.err_formatter ("%a@[" ^^ fmt ^^ "@]@.") pp_header (level, header, src)
  in
  { Logs.report }

let () =
  Logs.set_level ~all:true (Some Debug);
  Logs.set_reporter logs_reporter;
  Random.self_init ();

  Eio_main.run @@ fun env ->
  Eio.Fiber.with_binding current_env env @@ fun () ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _token ->
  Lwt_eio.run_lwt @@ fun () -> Alcotest_lwt.run "Scrutiny" [ T_scrutiny_infrastructure.tests ]
