type change =
  [ `Same
  | `Remove
  | `Add
  ]

type contents =
  | Lines of (change * string) list
  | Object of (string * t) list

and t = {
  has_entry : bool;
  contents : contents;
}

let is_empty x = not x.has_entry
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

module Structure = struct
  type 'a row = {
    name : string;
    diff : 'a -> 'a -> t;
    basic : change -> 'a -> t;
  }

  let row ~name ~pp ?(eq = ( = )) getter =
    let diff x y =
      let x = getter x and y = getter y in
      if eq x y then of_lines [ (`Same, pp x) ] else of_lines [ (`Remove, pp x); (`Add, pp y) ]
    in
    { name; diff; basic = (fun change x -> of_lines [ (change, pp (getter x)) ]) }

  let compare fields l r =
    match (l, r) with
    | None, None -> structure []
    | Some l, None -> structure (List.map (fun row -> (row.name, row.basic `Remove l)) fields)
    | None, Some r -> structure (List.map (fun row -> (row.name, row.basic `Add r)) fields)
    | Some l, Some r -> structure (List.map (fun row -> (row.name, row.diff l r)) fields)

  let map f { name; diff; basic } =
    { name; diff = (fun x y -> diff (f x) (f y)); basic = (fun c x -> basic c (f x)) }
end
