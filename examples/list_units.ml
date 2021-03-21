module Units = Scrutiny_systemd.Units

let () =
  Lwt_main.run
  @@
  let open Lwt.Syntax in
  let* bus = OBus_bus.system () in
  let* units = Scrutiny_systemd.Units.list_units bus in
  let+ units =
    units
    |> List.filter (fun (x : Units.unit_state) -> CCString.suffix ~suf:".service" x.id)
    |> Lwt_list.map_p (fun (x : Units.unit_state) ->
           let+ cgroup = Units.Service.control_group x in
           (x, cgroup))
  in
  units
  |> List.iter @@ fun ((x : Units.unit_state), cgroup) ->
     Printf.printf "%-50s (%s) -> %s\n" x.id x.active_state cgroup
