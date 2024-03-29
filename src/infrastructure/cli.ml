open Eio.Std
include Core
include Types
module KeyTbl = Hashtbl.Make (Core.Concrete_key.Boxed)

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

  (** Alternative to {!elapsed} which accepts a counter instead. *)
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

      let seconds = Mtime.Span.to_float_ns !latest *. 1e-9 in
      Line_buffer.add_string buf
      @@
      if seconds > 60.0 then
        Printf.sprintf "%02.0fm%02.0fs"
          (Float.div seconds 60. |> Float.floor)
          (Float.rem seconds 60. |> Float.floor)
      else Printf.sprintf "% 5.1fs" seconds
    in
    theta ~width:6 pp |> to_line

  (** A generic progress bar, used to display total progress for the whole sync. *)
  let progress_line ~total =
    list
      [
        spinner ();
        bar ~color:(ansi (`bright `cyan)) ~style:`UTF8 total;
        parens (elapsed ());
        brackets (count_to total);
      ]

  (** A progress line which displays the current status of a particular task. *)
  let key_line (Concrete_key.BKey key) counter =
    let task_name =
      match key with
      | Resource { resource; key; machine; _ } ->
          let module R = (val resource) in
          let task_name = constf "%a" R.pp key in
          let task_name =
            match machine with
            | Local -> task_name
            | Remote remote -> const ~color:(ansi `yellow) remote.host ++ const " » " ++ task_name
          in
          task_name
      | Var (CVar { var; _ }) ->
          let var_name =
            match var with
            | { name = Some name; _ } -> const name
            | { pos = Some (file, line, _, _); _ } -> constf "Var defined at %s:%d" file line
            | _ -> const "Unknown var"
          in
          (* TODO: Pretty-print context. *)
          var_name
      | Machine Local -> const "Local executor"
      | Machine (Remote r) -> constf "Tunnel to %s" r.host
    in
    list [ spinner (); task_name; const "»"; string; elapsed_since counter ]
end

type active_key = {
  clock : Mtime_clock.counter;
  mutable log_dirty : bool;
  mutable log : string;
  mutable line : string Progress.Reporter.t option;
}

module LogsHelpers = struct
  let capturing_reporter active_keys =
    let str_buffer = Buffer.create 256 in
    let str_format = Format.formatter_of_buffer str_buffer in

    let report _ _ ~over k msgf =
      let k _ = over (); k () in
      let emit key out =
        Format.pp_print_flush str_format ();
        let contents = Buffer.contents str_buffer in
        Buffer.reset str_buffer;
        key.log_dirty <- true;
        key.log <- contents;
        k out
      in

      msgf @@ fun ?header:_ ?(tags = Logs.Tag.empty) fmt ->
      match Logs.Tag.find Runner.Log_tag.tag tags with
      | None -> Format.ikfprintf k str_format fmt
      | Some key -> (
        match KeyTbl.find_opt active_keys key with
        | None -> Format.ikfprintf k str_format fmt
        | Some key -> Format.kfprintf (emit key) str_format fmt)
    in
    { Logs.report }

  let logs_reporter show_tag =
    let pp_level_with_src out style h src =
      Fmt.pf out "[%a/%s] " Fmt.(styled style string) h (Logs.Src.name src)
    in
    let pp_level_no_src out style h _ = Fmt.pf out "[%a] " Fmt.(styled style string) h in
    let pp_level = if show_tag then pp_level_with_src else pp_level_no_src in
    let pp_header out (l, h, s) =
      match l with
      | Logs.App -> (
        match h with
        | None -> ()
        | Some h -> pp_level out `Cyan h s)
      | Logs.Error -> pp_level out `Red (Option.value ~default:"ERROR" h) s
      | Logs.Warning -> pp_level out `Yellow (Option.value ~default:"WARNING" h) s
      | Logs.Info -> pp_level out `Blue (Option.value ~default:"INFO" h) s
      | Logs.Debug -> pp_level out `Green (Option.value ~default:"DEBUG" h) s
    in
    let pp_key out key =
      match key with
      | None -> ()
      | Some (Concrete_key.BKey key) -> Fmt.pf out "%a » " Concrete_key.pp key
    in

    let formatter = Format.err_formatter in
    let out_functions = Format.pp_get_formatter_out_functions formatter () in
    Format.pp_set_formatter_out_functions formatter
      { out_functions with out_newline = (fun () -> out_functions.out_string "\n\x1b[K" 0 4) };

    let report src level ~over k msgf =
      let k _ = over (); k () in
      msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
      let key = Logs.Tag.find Runner.Log_tag.tag tags in
      Format.kfprintf k Format.err_formatter
        ("%a@[%a" ^^ fmt ^^ "@]@.")
        pp_header (level, header, src) pp_key key
    in
    { Logs.report }

  let combine_reporters r1 r2 =
    let report src level ~over k msgf =
      let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
      r2.Logs.report src level ~over (fun () -> v) msgf
    in
    { Logs.report }
end

(** Create a progress display, which tracks total progress and displays long-running tasks. *)
let progress_tracker ~sw ~clock ~active_keys ~total =
  let progress_display =
    ProgressHelpers.progress_line ~total |> Progress.Multi.line |> Progress.Display.start
  in

  (* Unconditionally set the line. *)
  let set_line line message =
    let module String = Stdlib.String in
    let message =
      match String.index_opt message '\n' with
      | None -> message
      | Some p -> String.sub message 0 p
    in
    Progress.Reporter.report line message
  in
  (* Update the log line if needed. *)
  let update_line status =
    if status.log_dirty then (
      Option.iter (fun l -> set_line l status.log) status.line;
      status.log_dirty <- false)
  in

  let key_start key =
    KeyTbl.replace active_keys key
      { clock = Mtime_clock.counter (); line = None; log_dirty = false; log = "" }
  in
  let key_done key =
    (match KeyTbl.find_opt active_keys key with
    | Some ({ line = Some line; _ } as status) ->
        update_line status; Progress.Reporter.finalise line
    | _ -> ());
    KeyTbl.remove active_keys key;

    let [ reporter ] = Progress.Display.reporters progress_display in
    reporter 1
  in
  let progress = { Runner.key_start; key_done } in

  (* Tick the progress bar. *)
  let rec tick_progress () =
    KeyTbl.iter
      (fun key status ->
        let elapsed = Mtime_clock.count status.clock in
        (match status.line with
        | None when Mtime.Span.to_float_ns elapsed > 1e9 ->
            let line =
              Progress.Display.add_line ~above:1 progress_display
                (ProgressHelpers.key_line key status.clock)
            in
            set_line line status.log;
            status.line <- Some line;
            status.log_dirty <- false
        | _ -> ());

        update_line status)
      active_keys;

    Progress.Display.tick progress_display;
    Eio.Time.sleep clock 0.2;
    tick_progress ()
  in

  Fiber.fork_daemon ~sw tick_progress;
  Switch.on_release sw (fun () -> Progress.Display.finalise progress_display);
  progress

let setup_logs ?(extra_level = Logs.Warning) level =
  Printexc.record_backtrace true;
  Logs.set_level ~all:true (Some level);
  Logs.Src.list ()
  |> List.iter (fun src ->
         match Logs.Src.name src with
         | "piaf.client" | "piaf.http" | "piaf.openssl" -> Logs.Src.set_level src (Some extra_level)
         | "eio_linux" -> Logs.Src.set_level src None
         | _ -> ())

let main rules_def =
  Printexc.record_backtrace true;
  Fmt_tty.setup_std_outputs ();

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
      value @@ flag_all @@ info ~doc:"Print more verbose log messages." [ "verbose"; "v" ]
    in

    (* Set up logging. We parse the verbose flag to actually have two logging levels here, as some
       components are pretty noisy. *)
    let (level, extra_level) : Logs.level * Logs.level =
      match List.length verbose with
      | 0 -> (Warning, Warning)
      | 1 -> (Info, Warning)
      | _ -> (Debug, Debug)
    in
    setup_logs ~extra_level level;

    Eio_main.run @@ fun env ->
    let active_keys = KeyTbl.create 16 in

    (* Set up log handlers. We do this inside eio so that the effect handlers are installed. *)
    LogsHelpers.(combine_reporters (capturing_reporter active_keys) (logs_reporter (level = Debug)))
    |> Runner.Log_tag.wrap |> Progress.instrument_logs_reporter |> Logs.set_reporter;

    (* Evaluate rules. *)
    let rules = Builder_map.create 16 in
    rules_def { Rules.rules; context = { user = `Current; machine = Local } };

    (* And run everything! *)
    let ok =
      Switch.run @@ fun sw ->
      let progress =
        progress_tracker ~sw ~clock:env#clock ~active_keys ~total:(Builder_map.length rules)
      in
      Runner.apply ~env ~dry_run ~progress rules
    in
    match ok with
    | Error err ->
        Logs.err (fun f -> f "%s" err);
        exit 1
    | Ok { failed = 0; _ } -> exit 0
    | Ok { failed = n; _ } -> exit (min 126 n)
  in

  Cmd.v (Cmd.info Sys.executable_name ~doc) term |> Cmd.eval |> exit
