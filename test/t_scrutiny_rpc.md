## Scrutiny_rpc

```ocaml
# module R = Scrutiny_rpc ;;
module R = Scrutiny_rpc
```

Register our basic echo method:

```ocaml
# let m = R.Method.(make "echo" @@ string @-> returning string) ;;
val m : (string -> string, [ `Call ]) R.Method.t = <abstr>
# let h = R.Method.handle m @@ fun x -> Eio.traceln "echo %s" x; x ;;
val h : R.Method.handle = <abstr>
```

Let's perform a basic RPC call

```ocaml
# Eio_mock.Backend.run @@ fun () ->
  with_buffered_pipe "I/O[1]" @@ fun (source_1, sink_1) ->
  with_buffered_pipe "I/O[2]" @@ fun (source_2, sink_2) ->

  Switch.run @@ fun sw ->
    let _rpc_1 = R.create ~sw [h] source_1 sink_2 in
    let rpc_2 = R.create ~sw [] source_2 sink_1 in
    assert (R.call rpc_2 m "hello" = "hello")
  ;;
+I/O[1]: written "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"echo\",\"params\":[\"hello\"]}\n"
+I/O[1]: read "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"echo\",\"params\":[\"hello\"]}\n"
+echo hello
+I/O[2]: written "{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":\"hello\"}\n"
+I/O[2]: read "{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":\"hello\"}\n"
- : unit = ()
```

## Logging

scrutiny_rpc supports forwarding log messages over the channel.

First, register our logging methods:

```ocaml
# let log_h = R.Method.handle m @@ fun x -> Logs.info (fun f -> f "%s" x); x ;;
val log_h : R.Method.handle = <abstr>
```

Now we do our RPC call again. The server installs a `log_forwarder` after
startup. When calling a method on the client, logs are invoked as if coming
from that method.

```ocaml
# with_scoped_logger @@ fun logs ->
  Eio_mock.Backend.run @@ fun () ->

  with_buffered_pipe "I/O[1]" @@ fun (source_1, sink_1) ->
  with_buffered_pipe "I/O[2]" @@ fun (source_2, sink_2) ->

  Switch.run @@ fun sw ->
    let _rpc_1 = logs.run @@ fun set_log ->
      let rpc = R.create ~sw [log_h] source_1 sink_2 in
      set_log := R.log_forwarder rpc
    in

    let rpc_2 = R.create ~sw [] source_2 sink_1 in

    logs.run @@ fun set_log ->
      set_log := named_reporter "call-hello";
      assert (R.call rpc_2 m "hello" = "hello")
  ;;
+I/O[1]: written "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"echo\",\"params\":[\"hello\"]}\n"
+I/O[1]: read "{\"jsonrpc\":\"2.0\",\"id\":0,\"method\":\"echo\",\"params\":[\"hello\"]}\n"
+I/O[2]: written "{\"jsonrpc\":\"2.0\",\"method\":\"$/log\",\"params\":[0,[\"Info\"],\"application\",\"hello\"]}\n{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":\"hello\"}\n"
+I/O[2]: read "{\"jsonrpc\":\"2.0\",\"method\":\"$/log\",\"params\":[0,[\"Info\"],\"application\",\"hello\"]}\n{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":\"hello\"}\n"
call-hello: [INFO] hello
- : unit = ()
```
