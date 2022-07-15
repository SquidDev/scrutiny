type change =
  [ `Same
  | `Remove
  | `Add
  ]

type contents =
  | Lines of (change * string) list
  | Patch of (change * string array) list
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
    let lines =
      match String.split_on_char '\n' old with
      | [] -> []
      | xs -> [ (`Same, Array.of_list xs) ]
    in
    { has_entry = false; contents = Patch lines }
  else
    let diff = Str_diff.diff (String.split_on_char '\n' old) (String.split_on_char '\n' new_) in
    { has_entry = true; contents = Patch diff }

let structure xs =
  let has_entry = List.exists (fun (_, x) -> x.has_entry) xs in
  { has_entry; contents = Object xs }

module PP = struct
  let colour = function
    | `Same -> `White
    | `Remove -> `Hi `Red
    | `Add -> `Hi `Green

  let pp_line out change line =
    Format.pp_print_string out
      (match change with
      | `Same -> "  "
      | `Remove -> "- "
      | `Add -> "+ ");
    Fmt.styled (`Fg (colour change)) Fmt.string out line

  type 'a slice = {
    array : 'a array;
    start : int;
    length : int;
  }

  let pp_hunk out ~cut ~alo ~ahi ~blo ~bhi parts =
    let pp_header out () = Format.fprintf out "@@ -%d,%d +%d,%d @@" alo ahi blo bhi in
    if cut then Format.pp_print_space out ();
    Fmt.styled (`Fg `Magenta) pp_header out ();

    Fun.flip List.iter parts @@ fun (kind, { array; start; length }) ->
    for i = 0 to length - 1 do
      Format.pp_print_space out ();
      pp_line out kind array.(start + i)
    done

  let context = 3

  let pp_trailing out ~cut ~acc ~alo ~ahi ~blo ~bhi lines =
    let stop = Int.min context (Array.length lines) in
    let ahi = ahi + stop and bhi = bhi + stop in
    let acc = (`Same, { array = lines; start = 0; length = stop }) :: acc in
    pp_hunk out ~cut ~alo ~ahi ~blo ~bhi (List.rev acc)

  let pp_patch out lines =
    let rec go ~cut ~acc ~alo ~ahi ~blo ~bhi = function
      (* We finished on a Add/Remove, just display the hunk. *)
      | [] -> pp_hunk out ~cut ~alo ~ahi ~blo ~bhi (List.rev acc)
      (* We finished on an equal block. Take the first n lines and display the hunk. *)
      | [ (`Same, lines) ] -> pp_trailing out ~cut ~acc ~alo ~ahi ~blo ~bhi lines
      (* If we're a Same block, either finish a hunk or just add some context to the existing
         one. *)
      | (`Same, lines) :: xs ->
          let length = Array.length lines in
          if length > context * 2 then (
            pp_trailing out ~cut ~acc ~alo ~ahi ~blo ~bhi lines;

            let alo = ahi + length + (2 * context) in
            let blo = bhi + length + (2 * context) in
            let acc = [ (`Same, { array = lines; start = length - context; length = context }) ] in
            go ~cut:true ~acc ~alo ~ahi:alo ~blo ~bhi:blo xs)
          else
            let acc = (`Same, { array = lines; start = 0; length }) :: acc in
            go ~cut ~acc ~alo ~ahi:(ahi + length) ~blo ~bhi:(bhi + length) xs
      | (kind, lines) :: xs ->
          let length = Array.length lines in
          let ahi, bhi =
            match kind with
            | `Add -> (ahi + length, bhi)
            | `Remove -> (ahi, bhi + length)
            | _ -> assert false (* Can't be `Same *)
          in
          let acc = (kind, { array = lines; start = 0; length }) :: acc in
          go ~cut ~acc ~alo ~ahi ~blo ~bhi xs
    in
    match lines with
    | [ (`Same, _) ] -> ()
    | (`Same, lines) :: xs ->
        let length = Array.length lines in
        let start = max 0 (length - context) in
        let acc = [ (`Same, { array = lines; start; length = length - start }) ] in
        go ~cut:false ~acc ~alo:length ~ahi:length ~blo:length ~bhi:length xs
    | lines -> go ~cut:false ~acc:[] ~alo:1 ~ahi:1 ~blo:1 ~bhi:1 lines

  let rec pp_child ~full out = function
    | { has_entry = false; _ } when full = false -> ()
    | { contents = Lines xs; _ } ->
        Fun.flip List.iteri xs @@ fun i (change, line) ->
        if i > 0 then Format.pp_print_space out ();
        pp_line out change line
    | { contents = Patch xs; _ } ->
        if full then (
          Fun.flip List.iteri xs @@ fun i (change, lines) ->
          Fun.flip Array.iteri lines @@ fun j line ->
          if i > 0 || j > 0 then Format.pp_print_space out ();
          pp_line out change line)
        else pp_patch out xs
    | { contents = Object xs; _ } ->
        Fun.flip List.iter xs @@ fun (field, change) ->
        if full || change.has_entry then
          Format.fprintf out "%a:@,  @[<v>%a@]@,"
            Fmt.(styled (`Fg `Cyan) string)
            field (pp_child ~full) change
end

let pp ?(full = false) out = Format.fprintf out "@[<v>%a@]" (PP.pp_child ~full)

module Structure = struct
  type 'a field = {
    name : string;
    diff : 'a -> 'a -> t;
    basic : change -> 'a -> t;
  }

  let field ~name ~pp ?(eq = ( = )) getter =
    let diff x y =
      let x = getter x and y = getter y in
      if eq x y then of_lines [ (`Same, pp x) ] else of_lines [ (`Remove, pp x); (`Add, pp y) ]
    in
    { name; diff; basic = (fun change x -> of_lines [ (change, pp (getter x)) ]) }

  let diff fields l r =
    match (l, r) with
    | None, None -> structure []
    | Some l, None -> structure (List.map (fun field -> (field.name, field.basic `Remove l)) fields)
    | None, Some r -> structure (List.map (fun field -> (field.name, field.basic `Add r)) fields)
    | Some l, Some r -> structure (List.map (fun field -> (field.name, field.diff l r)) fields)

  let map f { name; diff; basic } =
    { name; diff = (fun x y -> diff (f x) (f y)); basic = (fun c x -> basic c (f x)) }
end
