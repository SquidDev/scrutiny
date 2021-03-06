(** A computation which either succeeds, fails or throws an exception. *)

type 'a t =
  | Ok of 'a
  | Error of string
  | Exception of string
[@@deriving yojson]

let run (f : unit -> ('a, string) result) : 'a t =
  match f () with
  | Ok x -> Ok x
  | Error e -> Error e
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Exception (Fmt.str "%a" Fmt.exn_backtrace (e, bt))

let run_lwt (f : unit -> ('a, string) result Lwt.t) =
  match%lwt f () with
  | Ok x -> Lwt.return (Ok x : _ t)
  | Error e -> Lwt.return (Error e : _ t)
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Lwt.return (Exception (Fmt.str "%a" Fmt.exn_backtrace (e, bt)))

let map f : 'a t -> 'b t = function
  | Ok x -> Ok (f x)
  | Error e -> Error e
  | Exception e -> Exception e

let log (module Log : Logs.LOG) l (err, exn) =
  match%lwt l with
  | Error e ->
      Log.err (fun f -> f "%s: %s" err e);
      Lwt.return_error ()
  | Exception e ->
      Log.err (fun f -> f "%s: %s" exn e);
      Lwt.return_error ()
  | Ok x -> Lwt.return_ok x
