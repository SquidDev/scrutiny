module M = Curl.Multi
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))
open Eio.Std

module Cancellable : sig
  type t

  val create : jobs:(unit -> unit) Eio.Stream.t -> (unit -> unit) -> t
  val cancel : t -> unit
end = struct
  type t = unit Promise.u

  let create ~jobs fn =
    let await, resolve = Promise.create ~label:"Cancellable" () in
    Eio.Stream.add jobs (fun () -> Fiber.first (fun () -> Promise.await await) fn);
    resolve

  let cancel r = Promise.resolve r ()
end

type t = {
  multi : Curl.Multi.mt;
  pending_requests : (Curl.t, Curl.curlCode Promise.u) Hashtbl.t;
}

let get_and_remove tbl key =
  match Hashtbl.find_opt tbl key with
  | None -> None
  | Some value -> Hashtbl.remove tbl key; Some value

let create ~sw ~clock ~net:_ =
  let multi = M.create () in
  let jobs = Eio.Stream.create Int.max_int in
  Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Stream.take jobs |> Fiber.fork ~sw
      done;
      `Stop_daemon);

  (* Keep track of any pending requests. *)
  let pending_requests = Hashtbl.create 32 in
  let finish_pending () =
    let rec loop () =
      match M.remove_finished multi with
      | None -> ()
      | Some (h, code) ->
          (match get_and_remove pending_requests h with
          | Some resolve ->
              Log.debug (fun f -> f "Finished request to %s" (Curl.get_effectiveurl h));
              Promise.resolve resolve code
          | None -> Log.err (fun f -> f "Orphaned request to %s" (Curl.get_effectiveurl h)));
          loop ()
    in
    loop ()
  in
  (* set_timer_function provides a mechanism for a one-shot timer. *)
  let timer_event = ref None in
  let wait_timer timeout () =
    Eio.Time.sleep clock (float_of_int timeout /. 1000.);
    timer_event := None;
    M.action_timeout multi;
    finish_pending ()
  in
  M.set_timer_function multi (fun timeout ->
      (match !timer_event with
      | Some event ->
          Cancellable.cancel event;
          timer_event := None
      | None -> ());
      timer_event := Some (Cancellable.create ~jobs (wait_timer timeout)));

  (* File descriptor waiting. *)
  let fd_waiters = Hashtbl.create 32 in
  let add_waiter fd fn = Hashtbl.add fd_waiters fd (Cancellable.create ~jobs fn) in
  let wait_readable fd () =
    while true do
      Eio_unix.await_readable fd;
      let (_ : int) = M.action multi fd M.EV_IN in
      finish_pending ()
    done
  in
  let wait_writable fd _ =
    while true do
      Eio_unix.await_writable fd;
      let (_ : int) = M.action multi fd M.EV_OUT in
      finish_pending ()
    done
  in
  M.set_socket_function multi (fun fd what ->
      (* Cancel existing waiter. *)
      get_and_remove fd_waiters fd |> Option.iter Cancellable.cancel;
      (* And resubscribe to events. *)
      match what with
      | M.POLL_REMOVE | M.POLL_NONE -> ()
      | M.POLL_IN -> add_waiter fd (wait_readable fd)
      | M.POLL_OUT -> add_waiter fd (wait_writable fd)
      | M.POLL_INOUT -> add_waiter fd (fun () -> Fiber.both (wait_readable fd) (wait_writable fd)));

  let handle = { multi; pending_requests } in
  Switch.on_release sw (fun () -> M.cleanup multi);
  handle

let perform t req =
  let await, resolve = Promise.create () in
  Hashtbl.add t.pending_requests req resolve;
  Log.debug (fun f -> f "Starting request to %s" (Curl.get_effectiveurl req));
  M.add t.multi req;

  let cleanup () =
    M.remove t.multi req;
    Hashtbl.remove t.pending_requests req
  in
  Fun.protect ~finally:cleanup (fun () -> Promise.await await)
