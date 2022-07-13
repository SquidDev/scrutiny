include Core
include Types

let apply_ ?progress ?dry_run rules : unit Lwt.t =
  Lwt_switch.with_switch @@ fun switch -> Runner.apply ?progress ~switch ?dry_run rules

let apply ?dry_run rules =
  let (), rules = rules { Core.Rules.rules = []; user = `Current; machine = Local } in
  apply_ ?dry_run rules

module ProgressHelpers = struct
  open Progress.Line
  open Progress.Color

  let spinner () = spinner ~color:(ansi (`bright `green)) ()

  let const ?color text =
    match color with
    | None -> const text
    | Some color ->
        let text = Terminal.Style.(code (fg color)) ^ text ^ Terminal.Style.(code none) in
        const text

  let constf ?color fmt = Format.kasprintf (const ?color) fmt

  let elapsed_since counter =
    let open Internals in
    let latest = ref Mtime.Span.zero in
    let finished = ref false in
    let pp buf e =
      (match e with
      | `tick | `report -> latest := Mtime_clock.count counter
      | `finish when not !finished ->
          latest := Mtime_clock.count counter;
          finished := true
      | `rerender | `finish -> ());

      let seconds = Mtime.Span.to_s !latest in
      Line_buffer.add_string buf
      @@ Printf.sprintf "%02.0fm%02.0fs"
           (Float.div seconds 60. |> Float.floor)
           (Float.rem seconds 60. |> Float.floor)
    in
    theta ~width:6 pp |> to_line

  let progress_line ~total =
    list
      [
        spinner ();
        bar ~color:(ansi (`bright `cyan)) ~style:`UTF8 total;
        parens (elapsed ());
        brackets (count_to total);
      ]

  let key_line (BKey (Resource { resource; key; machine; _ })) counter =
    let module R = (val resource) in
    let task_name = constf "%a" R.pp key in
    let task_name =
      match machine with
      | Local -> task_name
      | Remote remote -> const ~color:(ansi `yellow) remote.host ++ const " » " ++ task_name
    in
    list [ spinner (); task_name; const "»"; string; elapsed_since counter ]
end

let main rules =
  Printexc.record_backtrace true;
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Progress.logs_reporter ());
  let open Cmdliner in
  let ( let+ ) x f = Term.(const f $ x) in
  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b) in

  let doc = "Applies a state specified by a file." in
  let term : unit Term.t =
    let+ dry_run =
      let open Arg in
      value @@ flag
      @@ info
           ~doc:"Do not actually apply these states, only printing out what would have changed."
           [ "dry-run"; "d" ]
    and+ verbose =
      let open Arg in
      value @@ flag @@ info ~doc:"Print more verbose log messages." [ "verbose"; "v" ]
    in
    Logs.set_level ~all:true (Some (if verbose then Debug else Warning));

    let (), rules = rules { Core.Rules.rules = []; user = `Current; machine = Local } in

    let active_keys = KeyTbl.create 16 in
    let progress_lines = ref 1 in
    let progress_display =
      ProgressHelpers.progress_line ~total:(List.length rules)
      |> Progress.Multi.line |> Progress.Display.start
    in

    let key_start key = KeyTbl.replace active_keys key (Mtime_clock.counter (), None) in
    let key_done key =
      (match KeyTbl.find_opt active_keys key with
      | Some (_, Some key) -> Progress.Reporter.finalise key; decr progress_lines
      | None | Some (_, None) -> ());
      KeyTbl.remove active_keys key;

      let [ reporter ] = Progress.Display.reporters progress_display in
      reporter 1
    in

    Fun.protect ~finally:(fun () -> Progress.Display.finalise progress_display) @@ fun () ->
    let rec tick () =
      KeyTbl.iter
        (fun key (counter, line) ->
          let elapsed = Mtime_clock.count counter in
          match line with
          | None when Mtime.Span.to_s elapsed > 1.0 ->
              let line =
                Progress.Display.add_line ~above:!progress_lines progress_display
                  (ProgressHelpers.key_line key counter)
              in
              incr progress_lines;
              KeyTbl.replace active_keys key (counter, Some line)
          | _ -> ())
        active_keys;

      Progress.Display.tick progress_display;
      Lwt.bind (Lwt_unix.sleep 0.2) tick
    in
    let go () =
      Lwt.async tick;
      apply_ ~progress:{ key_start; key_done } ~dry_run rules
    in
    Lwt_main.run (go ())
  in
  Cmd.v (Cmd.info Sys.executable_name ~doc) term |> Cmd.eval |> exit

let run_tunnel () =
  Lwt_main.run (Lwt_switch.with_switch (fun switch -> Tunnel.run_tunnel ~switch ()))
