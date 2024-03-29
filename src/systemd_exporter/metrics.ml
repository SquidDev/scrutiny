open Eio.Std
module SMap = Map.Make (String)
module Manager = Scrutiny_systemd.Manager

module State = struct
  type t =
    | Activating
    | Active
    | Deactivating
    | Deactive
    | Failed
    | Inactive

  let all = [ Activating; Active; Deactivating; Deactive; Failed; Inactive ]

  let v = function
    | "activating" -> Activating
    | "active" -> Active
    | "deactivating" -> Deactivating
    | "deactive" -> Deactive
    | "failed" -> Failed
    | "inactive" -> Inactive
    | x -> failwith ("Unknown state " ^ x)

  let to_string = function
    | Activating -> "activating"
    | Active -> "active"
    | Deactivating -> "deactivating"
    | Deactive -> "deactive"
    | Failed -> "failed"
    | Inactive -> "inactive"
end

type unit_state = {
  mutable active : bool;
  mutable state : State.t;
  mutable memory_current : float option;
  mutable memory_anon : float option;
  mutable cpu : float option;
}

let units = ref SMap.empty

let get_unit name =
  match SMap.find_opt name !units with
  | Some x -> x
  | None ->
      let state =
        { active = false; state = Inactive; memory_current = None; memory_anon = None; cpu = None }
      in
      units := SMap.add name state !units;
      state

module Metrics = struct
  open Prometheus

  type t = MetricInfo.t * (unit -> Sample_set.sample list LabelSetMap.t)

  let unit_metric ~help name fn : t =
    let info =
      {
        MetricInfo.name = MetricName.v name;
        help;
        metric_type = Gauge;
        label_names = [ LabelName.v "name" ];
      }
    in
    let collect () =
      SMap.to_seq !units
      |> Seq.filter (fun (_, x) -> x.active)
      |> Seq.map (fun (id, x) ->
             let samples =
               match fn x with
               | None -> []
               | Some x -> [ Sample_set.sample x ]
             in
             ([ id ], samples))
      |> LabelSetMap.of_seq
    in
    (info, collect)

  let unit_current_memory : t =
    unit_metric ~help:"Current memory used by this unit" "systemd_unit_current_memory" (fun x ->
        x.memory_current)

  let unit_anon_memory : t =
    unit_metric ~help:"Current anonymous (i.e. non-mmaped) memory used by this unit."
      "systemd_unit_anon_memory" (fun x -> x.memory_anon)

  let unit_cpu_usage_nsec : t =
    unit_metric ~help:"Total CPU usage (us)" "systemd_unit_cpu_usage_usec" (fun x -> x.cpu)

  (** Exposes a metric for every state and unit, each being [1] when the unit is in that state and
      [0] otherwise.

      Yes, it's horrible, but Prometheus is designed for just numbers, so this is the most sensible
      way. *)
  let unit_state : t =
    let info =
      {
        MetricInfo.name = MetricName.v "systemd_unit_state";
        help = "Systemd unit";
        metric_type = Gauge;
        label_names = [ LabelName.v "name"; LabelName.v "state" ];
      }
    in
    let collect () =
      SMap.to_seq !units
      |> Seq.filter (fun (_, x) -> x.active)
      |> Seq.flat_map (fun (id, x) ->
             List.to_seq State.all
             |> Seq.map @@ fun state ->
                ( [ id; State.to_string state ],
                  [ Sample_set.sample (if state = x.state then 1.0 else 0.0) ] ))
      |> LabelSetMap.of_seq
    in
    (info, collect)

  let all = [ unit_current_memory; unit_anon_memory; unit_cpu_usage_nsec; unit_state ]
end

let () =
  let add (info, collector) = Prometheus.CollectorRegistry.(register default) info collector in
  List.iter add Metrics.all

type t = {
  busses : Scrutiny_systemd.Manager.connection list;
  cgroups : Cgroups.t;
}

let collect ~fs t =
  SMap.iter (fun _ x -> x.active <- false) !units;

  (* For all busses and all units. *)
  Switch.run @@ fun sw ->
  Fun.flip Fiber.List.iter t.busses @@ fun bus ->
  let units = Manager.of_bus ~sw bus |> Manager.list_units in
  Fun.flip Fiber.List.iter units @@ fun unit_info ->
  if not (String.ends_with ~suffix:".service" unit_info.id) then ()
  else
    let unit_state = get_unit unit_info.id in
    unit_state.active <- true;
    unit_state.state <- State.v unit_info.active_state;

    let cgroup = Manager.Service.of_unit unit_info.unit |> Manager.Service.get_control_group in
    if cgroup <> "" then (
      let cgroup =
        if String.length cgroup > 0 && cgroup.[0] = '/' then CCString.drop 1 cgroup else cgroup
      in
      let cgroup = Fpath.v cgroup in
      let memory_current = Cgroups.get_memory_current ~fs cgroup t.cgroups in
      let memory_anon = Cgroups.get_memory_anon ~fs cgroup t.cgroups in
      let cpu = Cgroups.get_cpu cgroup ~fs t.cgroups in

      unit_state.memory_current <- Option.map float_of_int memory_current;
      unit_state.memory_anon <- Option.map float_of_int memory_anon;
      unit_state.cpu <- Option.map (fun x -> float_of_int x) cpu)
    else (
      unit_state.memory_current <- None;
      unit_state.memory_anon <- None;
      unit_state.cpu <- None)
