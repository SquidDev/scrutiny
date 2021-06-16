type change =
  [ `Same
  | `Remove
  | `Add
  ]

type contents =
  | Lines of (change * string) list
  | Object of (string * t) list

and t =
  { has_entry : bool;
    contents : contents
  }

let empty = { has_entry = true; contents = Lines [] }

let of_lines xs =
  let has_entry =
    Fun.flip List.exists xs @@ function
    | `Same, _ -> false
    | (`Add | `Remove), _ -> true
  in
  { has_entry; contents = Lines xs }

let of_line ~old ~new_ =
  if old = new_ then { has_entry = false; contents = Lines [ (`Same, old) ] }
  else { has_entry = true; contents = Lines [ (`Remove, old); (`Add, new_) ] }

let of_diff ~old ~new_ =
  if old = new_ then
    let lines = String.split_on_char '\n' old in
    { has_entry = false; contents = Lines (List.map (fun x -> (`Same, x)) lines) }
  else
    let diff = Str_diff.diff (String.split_on_char '\n' old) (String.split_on_char '\n' new_) in
    { has_entry = true; contents = Lines diff }

let structure xs =
  let has_entry = List.exists (fun (_, x) -> x.has_entry) xs in
  { has_entry; contents = Object xs }

let colour = function
  | `Same -> `White
  | `Remove -> `Hi `Red
  | `Add -> `Hi `Green

let rec pp_child ~full out = function
  | { has_entry = false; _ } when full = false -> ()
  | { contents = Lines xs; _ } ->
      Fun.flip List.iteri xs @@ fun i (change, line) ->
      if i > 0 then Format.pp_print_space out ();
      Format.pp_print_string out
        (match change with
        | `Same -> "  "
        | `Remove -> "- "
        | `Add -> "+ ");
      Fmt.styled (`Fg (colour change)) Fmt.string out line
  | { contents = Object xs; _ } ->
      Fun.flip List.iter xs @@ fun (field, change) ->
      if full || change.has_entry then
        Format.fprintf out "%a:@,  @[<v>%a@]@,"
          Fmt.(styled (`Fg `Cyan) string)
          field (pp_child ~full) change

let pp ?(full = false) out = Format.fprintf out "@[<v>%a@]" (pp_child ~full)
