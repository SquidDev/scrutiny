module M = Scrutiny_systemd.Manager

let () =
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let* bus = OBus_bus.system () in
  let* units = M.list_units (M.of_bus bus) in
  let+ units =
    units
    |> List.filter (fun (x : M.unit_state) -> CCString.suffix ~suf:".service" x.id)
    |> Lwt_list.map_p (fun (x : M.unit_state) ->
           let+ cgroup = M.Service.of_unit x.unit_path |> M.Service.control_group in
           (x, cgroup))
  in
  units
  |> List.iter @@ fun ((x : M.unit_state), cgroup) ->
     Printf.printf "%-50s (%s) -> %s\n" x.id x.active_state cgroup
