open Import
open Eio.Std
module Log = (val Logs.src_log (Logs.Src.create __FILE__))

module I = struct
  let templates = Fpath.v "files"

  include Scrutiny_infrastructure
  include Scrutiny_infrastructure_resources
  include Rules
  include Action
  include File
  include Service

  let rec iter_rules f xs =
    match xs () with
    | Seq.Nil -> pure ()
    | Seq.Cons (x, xs) ->
        let* _ = f x in
        iter_rules f xs
end

let docker = Sys.getenv_opt "SCRUTINY_DOCKER" |> Option.value ~default:"podman"

(** Build our shared docker image and return the image hash. *)
let get_docker_image ~fs =
  let build_image () =
    with_temp ~fs @@ fun file ->
    run_command
      [|
        docker;
        "build";
        "--network=host";
        "-q";
        "--iidfile=" ^ snd file;
        "--file=support/Dockerfile";
      |];
    let id = Eio.Path.load file |> String.trim in
    Log.info (fun f -> f "Built systemd image as %S" id);
    if id = "" then failwith "Image ID is empty!";
    id
  in
  let image = Lazy.from_fun build_image in
  fun () -> Lazy.force image

(** Create a new container and pass the container id to the provided function. The container is
    automatically removed once the function exits. *)
let with_container ~(env : Eio_unix.Stdenv.base) fn : unit =
  Switch.run @@ fun sw ->
  let image = get_docker_image ~fs:env#fs () in
  let name = Printf.sprintf "scrutiny-systemd-%016Lx" (Random.bits64 ()) in
  Log.info (fun f -> f "Starting systemd container %S using image %S" name image);
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
    |];
  Fiber.fork ~sw (fun () -> run_command [| docker; "logs"; "-f"; name |]);
  run_command
    [|
      docker; "cp"; "../src/infrastructure/bin/tunnel.exe"; name ^ ":/usr/bin/scrutiny-infra-tunnel";
    |];

  (* Sleep a bit to wait for systemd to come up. *)
  Eio.Time.sleep env#clock 1.0;

  Fun.protect ~finally:(fun () -> run_command [| docker; "kill"; name |]) (fun () -> fn name)

(** Create a new container and apply a set of rules targetting that container. *)
let run_rules ~env ?(timeout = 10.0) ~name rules =
  let open I in
  let remote = Remote.make name in
  let rules =
    let* _ = with_remote remote rules in
    pure ()
  in

  match with_timeout ~clock:env#clock ~timeout @@ fun () -> apply ~env rules with
  | Error e -> Alcotest.fail e
  | Ok { failed = 0; changed; _ } -> changed
  | Ok { failed = n; _ } -> Alcotest.failf "%d rules failed to apply" n

(**********************************************************************************************************************)

let test_create_file ~env () =
  with_container ~env @@ fun name ->
  (* Create a file. *)
  let rules () =
    let open I in
    file' (Fpath.v "/root/test.txt") @@ fun () -> value "A test file"
  in
  (* Write it once. *)
  let changed = run_rules ~env ~name rules in
  Alcotest.(check int) "File should be created" 1 changed;

  (* Writing it again should not change anything. *)
  let changed = run_rules ~env ~name rules in
  Alcotest.(check int) "File should not be changed" 0 changed

let test_start_service ~env () =
  with_container ~env @@ fun name ->
  (* Create a service. *)
  let rules () =
    let open I in
    service ~name:"nginx" ~scope:`System @@ fun () ->
    value (service_state ~enabled:true ~running:true ~monitor:2 ())
  in
  (* Start the service once. *)
  let changed = run_rules ~env ~name rules in
  Alcotest.(check int) "Service should be started" 1 changed;

  (* Starting it again should not change anything. *)
  let changed = run_rules ~env ~name rules in
  Alcotest.(check int) "Service should not be changed" 0 changed

let test_restart_service ~env () =
  with_container ~env @@ fun name ->
  (* Create a service. *)
  let rules contents () =
    let open I in
    let* file = file' (Fpath.v "/root/test.txt") @@ fun () -> value contents in
    service ~name:"nginx" ~scope:`System @@ fun () ->
    let+ () = need ~options:`Restart file in
    service_state ~enabled:true ~running:true ~monitor:2 ()
  in
  (* Start the service once. *)
  let changed = run_rules ~env ~name @@ rules "Contents #1" in
  Alcotest.(check int) "Service should be started" 2 changed;

  (* Changing the file should restart the service *)
  let changed = run_rules ~env ~name @@ rules "Contents #2" in
  Alcotest.(check int) "Service should restart" 2 changed;

  (* Not changing the file should keep the service the same *)
  let changed = run_rules ~env ~name @@ rules "Contents #2" in
  Alcotest.(check int) "Service should not restart" 0 changed

let test_many_services ~env () =
  with_container ~env @@ fun name ->
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
  let changed = run_rules ~env ~name rules in
  Alcotest.(check int) "All Services should be started" (n + 1) changed

let tests ~env : unit Alcotest.test =
  let open Alcotest in
  ( "Infrastructure",
    [
      test_case "Write text file" `Slow (test_create_file ~env);
      test_case "Start systemd service" `Slow (test_start_service ~env);
      test_case "Restart systemd service" `Slow (test_restart_service ~env);
      test_case "Start many services" `Slow (test_many_services ~env);
    ] )
