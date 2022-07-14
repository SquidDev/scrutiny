open Import
module Log = (val Logs.src_log (Logs.Src.create __FILE__))

module I = struct
  let templates = Fpath.v "files"

  include Scrutiny_infrastructure
  include Scrutiny_infrastructure_resources
  include Rules
  include Action
  include File_resource
  include Service_resource

  let rec iter_rules f xs =
    match xs () with
    | Seq.Nil -> pure ()
    | Seq.Cons (x, xs) ->
        let* _ = f x in
        iter_rules f xs
end

let docker = Sys.getenv_opt "SCRUTINY_DOCKER" |> Option.value ~default:"podman"

(** Build our shared docker image and return the image hash. *)
let get_docker_image =
  let build_image () =
    with_temp @@ fun file ->
    let%lwt () =
      run_command
        [|
          docker; "build"; "--network=host"; "-q"; "--iidfile=" ^ file; "--file=support/Dockerfile";
        |]
    in
    let id = In_channel.with_open_text file In_channel.input_all |> String.trim in
    Log.info (fun f -> f "Built systemd image as %S" id);
    if id = "" then failwith "Image ID is empty!";
    Lwt.return id
  in
  let image = Lazy.from_fun build_image in
  fun () -> Lazy.force image

(** Create a new container and pass the container id to the provided function. The container is
    automatically removed once the function exits. *)
let with_container fn : unit Lwt.t =
  let%lwt image = get_docker_image () in
  let name = Printf.sprintf "scrutiny-systemd-%016Lx" (Random.bits64 ()) in
  Log.info (fun f -> f "Starting systemd container %S using image %S" name image);
  let%lwt () =
    run_command
      [|
        docker;
        "run";
        "-dt";
        "--network=none";
        "--tmpfs=/run";
        "--tmpfs=/tmp";
        "--name=" ^ name;
        "--rm";
        image;
      |]
  in
  Lwt.async (fun () -> run_command [| docker; "logs"; "-f"; name |]);
  let%lwt () =
    run_command
      [|
        docker;
        "cp";
        "../src/infrastructure/bin/tunnel.exe";
        name ^ ":/usr/bin/scrutiny-infra-tunnel";
      |]
  in

  Lwt.finalize (fun () -> fn name) (fun () -> run_command [| docker; "kill"; name |])

(** Create a new container and apply a set of rules targetting that container. *)
let run_rules ?(timeout = 10.0) ~name rules =
  let open I in
  let remote = Remote.make name in
  let rules =
    let* _ = with_remote remote rules in
    pure ()
  in
  match%lwt with_timeout ~timeout @@ fun () -> apply rules with
  | Error e -> Alcotest.fail e
  | Ok { failed = 0; changed; _ } -> Lwt.return changed
  | Ok { failed = n; _ } -> Alcotest.failf "%d rules failed to apply" n

(**********************************************************************************************************************)

let test_create_file (_ : Lwt_switch.t) () =
  with_container @@ fun name ->
  (* Create a file. *)
  let rules () =
    let open I in
    file' (Fpath.v "/root/test.txt") @@ fun () -> value "A test file"
  in
  (* Write it once. *)
  let%lwt changed = run_rules ~name rules in
  Alcotest.(check int) "File should be created" 1 changed;

  (* Writing it again should not change anything. *)
  let%lwt changed = run_rules ~name rules in
  Alcotest.(check int) "File should not be changed" 0 changed;

  Lwt.return_unit

let test_start_service (_ : Lwt_switch.t) () =
  with_container @@ fun name ->
  (* Create a service. *)
  let rules () =
    let open I in
    service ~name:"nginx" ~scope:`System @@ fun () ->
    value (service_state ~enabled:true ~running:true ~monitor:2 ())
  in
  (* Start the service once. *)
  let%lwt changed = run_rules ~name rules in
  Alcotest.(check int) "Service should be started" 1 changed;

  (* Starting it again should not change anything. *)
  let%lwt changed = run_rules ~name rules in
  Alcotest.(check int) "Service should not be changed" 0 changed;

  Lwt.return_unit

let test_restart_service (_ : Lwt_switch.t) () =
  with_container @@ fun name ->
  (* Create a service. *)
  let rules contents () =
    let open I in
    let* file = file' (Fpath.v "/root/test.txt") @@ fun () -> value contents in
    service ~name:"nginx" ~scope:`System @@ fun () ->
    let+ () = need ~options:`Restart file in
    service_state ~enabled:true ~running:true ~monitor:2 ()
  in
  (* Start the service once. *)
  let%lwt changed = run_rules ~name @@ rules "Contents #1" in
  Alcotest.(check int) "Service should be started" 2 changed;

  (* Changing the file should restart the service *)
  let%lwt changed = run_rules ~name @@ rules "Contents #2" in
  Alcotest.(check int) "Service should restart" 2 changed;

  (* Not changing the file should keep the service the same *)
  let%lwt changed = run_rules ~name @@ rules "Contents #2" in
  Alcotest.(check int) "Service should not restart" 0 changed;

  Lwt.return_unit

let test_many_services (_ : Lwt_switch.t) () =
  with_container @@ fun name ->
  (* Create a service. *)
  let n = 50 in
  let rules () =
    let open I in
    let* service_file =
      template
        (Fpath.v "/etc/systemd/system/sleep@.service")
        ~template:Fpath.(templates / "sleep@.service")
      @@ fun () -> value []
    in
    CCSeq.range 1 n
    |> iter_rules @@ fun i ->
       service ~name:(Printf.sprintf "sleep@%d" i) ~scope:`System @@ fun () ->
       let+ () = need service_file in
       service_state ~enabled:true ~running:true ~monitor:1 ()
  in
  (* Start the services once. *)
  let%lwt changed = run_rules ~name rules in
  Alcotest.(check int) "All Services should be started" (n + 1) changed;

  Lwt.return_unit

let tests : unit Alcotest_lwt.test =
  let open Alcotest_lwt in
  ( "Infrastructure",
    [
      test_case "Write text file" `Slow test_create_file;
      test_case "Start systemd service" `Slow test_start_service;
      test_case "Restart systemd service" `Slow test_restart_service;
      test_case "Start many services" `Slow test_many_services;
    ] )
