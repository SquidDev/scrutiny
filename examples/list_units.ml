module M = Scrutiny_systemd.Manager

let () =
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  Lwt_switch.with_switch @@ fun sw ->
  let bus = M.of_bus ~sw `User in
  let* () =
    let* emacs = M.load_unit bus "emacs.service" in
    let* () = M.Unit.enable emacs in
    let* () = M.daemon_reload bus in
    Lwt.return_unit
  in

  let* units = M.list_units bus in
  let+ units =
    units
    |> List.filter (fun (x : M.unit_state) -> CCString.suffix ~suf:".service" x.id)
    |> Lwt_list.map_p (fun (x : M.unit_state) ->
           let+ cgroup = M.Service.of_unit x.unit |> M.Service.get_control_group in
           (x, cgroup))
  in
  units
  |> List.iter @@ fun ((x : M.unit_state), cgroup) ->
     Printf.printf "%-50s (%s) -> %s\n" x.id x.active_state cgroup
