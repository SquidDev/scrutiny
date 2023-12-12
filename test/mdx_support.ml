include Eio.Std

type exn += Finished

let () =
  Printexc.record_backtrace true;
  Logs.set_level ~all:true (Some Info);
  Logs.Src.list ()
  |> List.iter (fun src ->
         match Logs.Src.name src with
         | "Curl_eio" -> Logs.Src.set_level src (Some Debug)
         | _ -> ());
  Logs.format_reporter () |> Logs.set_reporter

module Pipe = struct
  type contents = {
    body : string;
    mutable offset : int;
  }

  type queue = {
    name : string;
    sw : Switch.t;
    queue : contents Queue.t;
    condition : Eio.Condition.t;
  }

  module Two_way = struct
    type t = queue

    let rec single_read ({ sw; name; queue; condition } as t) sink =
      Switch.check sw;
      match Queue.peek_opt queue with
      | None ->
          Eio.Condition.await_no_mutex condition;
          single_read t sink
      | Some ({ body; offset } as buf) ->
          let body_len = String.length body in
          let len = min (body_len - offset) (Cstruct.length sink) in

          traceln "%s: read %S" name (String.sub body offset len);
          Cstruct.blit_from_string body offset sink 0 len;
          if len + offset >= body_len then Queue.take queue |> ignore
          else buf.offset <- offset + len;
          len

    let read_methods = []

    let single_write { sw = _; name; queue; condition } bufs =
      List.fold_left
        (fun acc x ->
          let read = Cstruct.to_string x in
          Queue.add { body = read; offset = 0 } queue;
          traceln "%s: written %S" name read;
          Eio.Condition.broadcast condition;
          acc + Cstruct.length x)
        0 bufs

    let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  end

  let source = Eio.Flow.Pi.source (module Two_way)
  let sink = Eio.Flow.Pi.sink (module Two_way)

  let create ~sw name : _ Eio.Flow.source * _ Eio.Flow.sink =
    let queue = { sw; name; queue = Queue.create (); condition = Eio.Condition.create () } in
    (T (queue, source), T (queue, sink))
end

let pipe = Pipe.create

let with_buffered_pipe name fn =
  Switch.run @@ fun sw ->
  let source, sink = pipe ~sw name in
  fn (Eio.Buf_read.of_flow ~max_size:(1024 * 1024) source, sink)

let with_sw f = try Switch.run @@ fun sw -> f sw; raise Finished with Finished -> ()

type 'arg scoped = { run : 'a. ('arg -> 'a) -> 'a }

let current_logger = Fiber.create_key ()

(** Fallback handler which always returns a Fiber context, which is useful for when working with
    fiber storage. Unclear if this is actually useful though! *)
let _fallback_handler (type a) () : a Effect.Deep.effect_handler =
  let context = Eio.Private.Fiber_context.make_root () in
  let effc (type b) : b Effect.t -> ((b, a) Effect.Deep.continuation -> a) option = function
    | Eio.Private.Effects.Get_context -> Some (fun k -> Effect.Deep.continue k context)
    | _ -> None
  in
  { effc }

let with_scoped_logger fn =
  let prev_reporter = Logs.reporter () in
  let get_reporter () = Fiber.get current_logger |> Option.fold ~none:prev_reporter ~some:( ! ) in

  let report src lvl ~over cb msgf =
    let reporter = get_reporter () in
    reporter.report src lvl ~over cb msgf
  in
  let run fn =
    let reporter = get_reporter () in
    let cell = ref reporter in
    Fiber.with_binding current_logger cell @@ fun () -> fn cell
  in

  Fun.protect ~finally:(fun () -> Logs.set_reporter prev_reporter) @@ fun () ->
  Logs.set_reporter { report };
  fn { run }

let named_reporter prefix =
  let pp_header out (l, h) =
    match h with
    | None -> Format.fprintf out "%s: [%a] " prefix Logs.pp_level l
    | Some h -> Format.fprintf out "%s: [%s] " prefix h
  in

  Logs.format_reporter ~pp_header ()
