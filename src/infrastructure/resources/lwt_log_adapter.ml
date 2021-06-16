module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

let conv_level : Lwt_log.level -> Logs.level = function
  | Debug -> Debug
  | Info | Notice -> Info
  | Warning -> Warning
  | Error | Fatal -> Error

let output _section level lines : unit Lwt.t =
  let level = conv_level level in
  Log.msg level (fun f -> f "%s" (String.concat "\n" lines));
  Lwt.return_unit

let logger = Lwt_log.make ~output ~close:(fun () -> Lwt.return_unit)

let setup () =
  Lwt_log.load_rules "* -> debug";
  Lwt_log.default := logger
