open Eio.Std
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

module Channel = struct
  type 'a t = {
    lock : Mutex.t;
    queue : 'a Queue.t;
    not_empty : Condition.t;
  }

  let create () =
    { lock = Mutex.create (); queue = Queue.create (); not_empty = Condition.create () }

  let push { lock; queue; not_empty } value =
    Mutex.lock lock; Queue.add value queue; Condition.signal not_empty; Mutex.unlock lock

  let take { lock; queue; not_empty } =
    Mutex.lock lock;
    while Queue.is_empty queue do
      Condition.wait not_empty lock
    done;
    let value = Queue.take queue in
    Mutex.unlock lock; value
end

type task = {
  name : string;
  fn : unit -> unit;
}

let queue = Channel.create ()
let thread_started = Atomic.make false

let worker () =
  while true do
    let { name; fn } = Channel.take queue in
    let start = Mtime_clock.counter () in

    fn ();

    let duration = Mtime_clock.count start in
    if Mtime.Span.to_ms duration > 10.0 then
      Log.warn (fun f -> f "%s took longer than expected: %a" name Mtime.Span.pp duration)
  done

let run ~name fn =
  Fiber.check ();
  if Atomic.compare_and_set thread_started false true then Thread.create worker () |> ignore;

  let await, resolve = Promise.create () in
  let fn () =
    match fn () with
    | x -> Promise.resolve_ok resolve x
    | exception e -> Promise.resolve_error resolve e
  in
  Channel.push queue { name; fn };
  Promise.await_exn await

let stat name = run ~name:"stat" @@ fun () -> Unix.stat name
let chown name uid gid = run ~name:"chown" @@ fun () -> Unix.chown name uid gid
let chmod name perms = run ~name:"chmod" @@ fun () -> Unix.chmod name perms
let getpwnam name = run ~name:"getpwnam" @@ fun () -> Unix.getpwnam name
let getgrnam name = run ~name:"getgrnam" @@ fun () -> Unix.getgrnam name
let getpwuid name = run ~name:"getpwuid" @@ fun () -> Unix.getpwuid name
