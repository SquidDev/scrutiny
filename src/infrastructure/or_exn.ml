(** A computation which either succeeds, fails or throws an exception. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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

let map f : 'a t -> 'b t = function
  | Ok x -> Ok (f x)
  | Error e -> Error e
  | Exception e -> Exception e

let log (module Log : Logs.LOG) (l : _ t) (err, exn) : _ result =
  match l with
  | Error e ->
      Log.err (fun f -> f "%s: %s" err e);
      Error ()
  | Exception e ->
      Log.err (fun f -> f "%s: %s" exn e);
      Error ()
  | Ok x -> Ok x
