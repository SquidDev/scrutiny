open Eio.Std
module Stream = Eio.Stream
module ITbl = Hashtbl.Make (CCInt)
module STbl = CCHashtbl.Make (String)
module Log = (val Logs.src_log (Logs.Src.create __MODULE__))

exception Rpc_exception = Json_rpc.Rpc_exception
exception Remote_exn = Json_rpc.Remote_exn

module type VALUE = Ppx_yojson_conv_lib.Yojsonable.S

module Method = struct
  open Ppx_yojson_conv_lib.Yojson_conv

  type 'a value = (module VALUE with type t = 'a)

  let value (type a) from to_ : a value =
    (module struct
      type nonrec t = a

      let t_of_yojson = from
      let yojson_of_t = to_
    end)

  let unit = value unit_of_yojson yojson_of_unit
  let int = value int_of_yojson yojson_of_int
  let bool = value bool_of_yojson yojson_of_bool
  let string = value string_of_yojson yojson_of_string

  type log_level = Logs.level =
    | App
    | Error
    | Warning
    | Info
    | Debug
  [@@deriving yojson]

  let log_level = value log_level_of_yojson yojson_of_log_level

  let option (type a) ((module V) : a value) =
    value (option_of_yojson V.t_of_yojson) (yojson_of_option V.yojson_of_t)

  let result (type o e) ((module O) : o value) ((module E) : e value) =
    let result_of_yojson : Yojson.Safe.t -> _ = function
      | `Assoc [ ("ok", x) ] -> Ok (O.t_of_yojson x)
      | `Assoc [ ("err", x) ] -> Error (E.t_of_yojson x)
      | x -> of_yojson_error "pair_of_yojson: invalid format" x
    in
    let yojson_of_result : _ -> Yojson.Safe.t = function
      | Ok x -> `Assoc [ ("ok", O.yojson_of_t x) ]
      | Error x -> `Assoc [ ("err", E.yojson_of_t x) ]
    in
    value result_of_yojson yojson_of_result

  let pair (type a b) ((module A) : a value) ((module B) : b value) =
    value (pair_of_yojson A.t_of_yojson B.t_of_yojson) (yojson_of_pair A.yojson_of_t B.yojson_of_t)

  type ('s, 'u) signature =
    | Returns : 'r value -> ('r, [ `Call ]) signature
    | Notify : (unit, [ `Notify ]) signature
    | Function : 'a value * ('r, 'u) signature -> ('a -> 'r, 'u) signature

  let returning arg = Returns arg
  let notify = Notify
  let ( @-> ) a f = Function (a, f)

  type ('s, 'u) t = {
    name : string;
    signature : ('s, 'u) signature;
  }

  let make name signature = { name; signature }

  type handle = Handle : ('s, 'u) t * 's -> handle

  let handle mth fn = Handle (mth, fn)
end

let current_method = Fiber.create_key ()
let log_method = Method.(make "$/log" (option int @-> log_level @-> string @-> string @-> notify))
let cancel_method = Method.(make "$/cancel" (int @-> string @-> notify))

let pp_exception out = function
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, json) ->
      Fmt.pf out "%a in %a" Fmt.exn e (Yojson.Safe.pretty_print ~std:false) json
  | e -> Fmt.exn out e

type t = {
  call : 'a 'u. ('a, 'u) Method.t -> 'a;
  is_closed : unit Promise.t;
}

(** A deferred function application. Equivalent to wrapping a function call in thunks, just easier
    to chain. *)
type 'a apply =
  | Fn : 'a -> 'a apply
  | Apply : ('x -> 'r) apply * 'x -> 'r apply

let rec run_apply : type a. a apply -> a = function
  | Fn x -> x
  | Apply (f, x) -> run_apply f x

(* An in progress method call. *)
type in_progress = {
  resolve : (Yojson.Safe.t, exn) result Promise.u;
  cancel : Eio.Cancel.t;
  log : (Logs.level * Logs.Src.t * string) Eio.Stream.t; (* Log messages to report. *)
}

let create ~sw methods input output =
  (* Set up our map of incoming methods. *)
  let method_tbl = STbl.create 8 in
  let add_handler (Method.Handle (mth, _) as handle) = STbl.replace method_tbl mth.name handle in
  List.iter add_handler methods;

  let is_closed, set_closed = Promise.create ~label:"RPC closed" () in

  (* We keep a queue of messages to send, then pull these in on a separate fiber. *)
  let send_queue : Json_rpc.message Stream.t = Stream.create 16 in
  let send = Stream.add send_queue in
  let sender () =
    let buf = Buffer.create 1024 in
    Eio.Buf_write.with_flow output @@ fun output ->
    while true do
      let msg = Stream.take send_queue in
      Json_rpc.yojson_of_message msg
      |> Yojson.Safe.to_string ~buf ~suf:"\n"
      |> Eio.Buf_write.string output
    done;
    `Stop_daemon
  in
  Fiber.fork_daemon ~sw sender;

  (* Keep a table of pending remote methods, and the id of the next one. *)
  let pending_methods = ITbl.create 32 in
  let next_id = Atomic.make 0 in

  (* Handle cancels. *)
  add_handler
    ( Method.handle cancel_method @@ fun id reason ->
      match ITbl.find_opt pending_methods id with
      | None -> ()
      | Some { cancel; _ } -> Eio.Cancel.cancel cancel (Remote_exn (reason, "")) );

  (* Forward log messages to the appropriate channel. *)
  let log_src_cache = STbl.create 16 in
  let find_log_src src =
    Logs.Src.list ()
    |> List.find_opt (fun x -> Logs.Src.name x = src)
    |> CCOption.get_lazy (fun () -> Logs.Src.create src)
  in
  add_handler
    ( Method.handle log_method @@ fun id level src msg ->
      let src = STbl.get_or_add log_src_cache ~f:find_log_src ~k:src in
      match Option.bind id (ITbl.find_opt pending_methods) with
      | None -> Logs.msg ~src level (fun f -> f "%s" msg)
      | Some { log; _ } -> Eio.Stream.add log (level, src, msg) );

  (* The handler for method calls - builds up a list of arguments and then sends it to the
     remote. *)
  let rec call_k : type f u. string -> Yojson.Safe.t list -> (f, u) Method.signature -> f =
   fun method_ xs signature ->
    match signature with
    | Function ((module A), args) -> fun a -> call_k method_ (A.yojson_of_t a :: xs) args
    | Returns (module R) -> (
        Eio.Cancel.sub @@ fun cc ->
        (* Acquire a method id, create a promise and register our waiter. *)
        let id = Atomic.fetch_and_add next_id 1 in
        let wait, resolve = Promise.create ~label:method_ () in
        let log = Eio.Stream.create Int.max_int in
        ITbl.replace pending_methods id { resolve; cancel = cc; log };
        (* Send the call to the remote. *)
        send (Call { id = Some id; method_; params = List.rev xs });

        (* And wait on the promise. *)
        let go () =
          Switch.run @@ fun sw ->
          Fiber.fork_daemon ~sw (fun () ->
              (* Spit out log messages while possible. *)
              while true do
                let level, src, msg = Eio.Stream.take log in
                Logs.msg ~src level (fun f -> f "%s" msg)
              done;
              assert false (* Unreachable! *));
          Promise.await_exn wait
        in
        let result =
          try go ()
          with Eio.Cancel.Cancelled e ->
            call_k cancel_method.name [] cancel_method.signature id (Printexc.to_string e);
            go ()
        in
        try R.t_of_yojson result
        with e ->
          Log.err (fun f -> f "Invalid method result %a" pp_exception e);
          raise (Rpc_exception "Invalid method result"))
    | Notify ->
        (* Notify is much simpler as we don't need to worry about ids. *)
        send (Call { id = None; method_; params = List.rev xs })
  in

  let resolve_method id value =
    match ITbl.find_opt pending_methods id with
    | Some { resolve; _ } -> ITbl.remove pending_methods id; Promise.resolve resolve value
    | None -> Log.err (fun f -> f "Receiving unknown method %d" id)
  in
  (* Dispatch an incoming call, running it on a new fiber. *)
  let with_current_method id f =
    match id with
    | None -> Fiber.without_binding current_method f
    | Some id -> Fiber.with_binding current_method id f
  in
  let dispatch_call (type a) id ((module R) : a Method.value) fn =
    Fiber.fork ~sw @@ fun () ->
    with_current_method id @@ fun () ->
    match run_apply fn with
    | value -> (
      match id with
      | None -> ()
      | Some id -> send (Result { id; result = R.yojson_of_t value }))
    | exception e ->
        let bt = Printexc.get_backtrace () in
        let e = Printexc.to_string e in
        send (Json_rpc.error ?id ~data:(`String bt) OCaml_exception e)
  in
  (* Parse arguments for an incoming call, then dispatch it. *)
  let rec parse_call :
      type s u.
      ?id:int -> (s, u) Method.signature -> Yojson.Safe.t list -> s apply -> Json_rpc.message option
      =
   fun ?id mth args fn ->
    match (mth, args) with
    | Returns _, _ :: _ | Notify, _ :: _ ->
        Some (Json_rpc.error ?id InvalidParams "Too many parameters to method call.")
    | Function _, [] ->
        Some (Json_rpc.error ?id InvalidParams "Not enough parameters to method call.")
    | Function ((module A), mth), arg :: args -> (
      match A.t_of_yojson arg with
      | value -> parse_call ?id mth args (Apply (fn, value))
      | exception e ->
          Log.err (fun f -> f "Received malformed argument %a" pp_exception e);
          Some (Json_rpc.error ?id InvalidParams "Malformed argument."))
    | Returns ret, [] -> dispatch_call id ret fn; None
    | Notify, [] -> dispatch_call id Method.unit fn; None
  in
  (* Parse an incoming message and handle it as appropriate. *)
  let handle_message line =
    match Yojson.Safe.from_string line with
    | exception e ->
        Log.err (fun f -> f "Receiving invalid JSON (%a)" pp_exception e);
        Some (Json_rpc.error ParseError "Cannot parse line")
    | json -> (
      match Json_rpc.message_of_yojson json with
      | exception e ->
          Log.err (fun f -> f "Receiving invalid message (%a)" pp_exception e);
          Some (Json_rpc.error InvalidRequest "Invalid message JSON")
      (* Results and errors are simple enough. *)
      | Result { id; result } -> resolve_method id (Ok result); None
      | Error { id = Some id; error } ->
          resolve_method id (Json_rpc.exn_of_error error |> Result.error);
          None
      | Error { id = None; error } ->
          Log.err (fun f -> f "Error on remote: %s" error.message);
          None
      | Call { id; method_; params } -> (
        match STbl.find_opt method_tbl method_ with
        | None ->
            Some (Json_rpc.error ?id MethodNotFound (Printf.sprintf "Method %s not found" method_))
        | Some (Handle (mth, fn)) -> parse_call ?id mth.signature params (Fn fn)))
  in
  (* The main reader loop - consume messages until the end of the file. *)
  let rec read () =
    match Eio.Buf_read.line input with
    | line ->
        (match handle_message line with
        | None -> ()
        | Some response -> send response);
        read ()
    | exception End_of_file -> `Stop_daemon
  in
  let read () = Fun.protect ~finally:(fun () -> Promise.resolve set_closed ()) read in
  Fiber.fork_daemon ~sw read;

  { call = (fun (meth : _ Method.t) -> call_k meth.name [] meth.signature); is_closed }

let call { call; _ } = call
let notify = call

let log_forwarder t =
  let owner_thread = Thread.self () |> Thread.id in
  let report src level ~over k msgf =
    let k msg =
      let src = Logs.Src.name src in
      let current_method =
        (* Only call Fiber.get on the Eio thread. *)
        if Thread.id (Thread.self ()) = owner_thread then Fiber.get current_method else None
      in
      notify t log_method current_method level src msg;
      over ();
      k ()
    in
    msgf @@ fun ?header:_ ?tags:_ fmt -> Format.kasprintf k fmt
  in
  { Logs.report }

let await_closed { is_closed; _ } = Promise.await is_closed
