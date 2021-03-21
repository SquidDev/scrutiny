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

let structure xs =
  let has_entry = List.exists (fun (_, x) -> x.has_entry) xs in
  { has_entry; contents = Object xs }

let colour = function
  | `Same -> `White
  | `Remove -> `Hi `Red
  | `Add -> `Hi `Green

let rec pp ?(full = false) out = function
  | { has_entry = false; _ } when full = false -> ()
  | { contents = Lines xs; _ } ->
      Fun.flip List.iter xs @@ fun (change, line) ->
      Format.pp_print_space out ();
      Fmt.styled (`Fg (colour change)) Fmt.string out line
  | { contents = Object xs; _ } ->
      Fun.flip List.iter xs @@ fun (field, change) ->
      if full || change.has_entry then
        Format.fprintf out "@[<v 2>%a:%a@]@,"
          Fmt.(styled (`Fg `Cyan) string)
          field (pp ~full) change
