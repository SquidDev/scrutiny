(* A very basic diffing algorithm. We really should optimise this and support displaying partial
   diffs. *)

module Slice = struct
  type 'a t = {
    array : 'a array;
    start : int;
    length : int;
  }

  let of_array array = { array; start = 0; length = Array.length array }
  let of_list xs = of_array (Array.of_list xs)
  let length x = x.length
  let slice ~from ~length { array; start; _ } = { array; start = start + from; length }

  let to_array { array; start; length } =
    if start = 0 && length = Array.length array then array else Array.sub array start length

  let iteri f { array; start; length } =
    for i = 0 to length - 1 do
      f i array.(start + i)
    done
end

let add kind slice out = if Slice.length slice = 0 then out else (kind, Slice.to_array slice) :: out

let rec diff_ olds news out =
  match (Slice.length olds, Slice.length news) with
  | 0, 0 -> out
  | 0, _ -> add `Add news out
  | _, 0 -> add `Remove olds out
  | _, _ ->
      (* Build up a map of all lines contents to their positions. *)
      let old_index_map = Hashtbl.create (Slice.length olds / 2) in
      Slice.iteri (fun i x -> Hashtbl.add old_index_map x i) olds;

      let overlap = Array.make (Slice.length olds) 0 in
      let sub_length = ref 0 and sub_range = ref (0, 0) in

      news
      |> Slice.iteri (fun new_index value ->
             let this_overlap = Array.make (Slice.length olds) 0 in
             Hashtbl.find_all old_index_map value
             |> List.iter (fun old_index ->
                    let this = if old_index = 0 then 1 else overlap.(old_index - 1) + 1 in
                    this_overlap.(old_index) <- this;
                    if this > !sub_length then (
                      sub_length := this;
                      sub_range := (old_index - this + 1, new_index - this + 1)));
             Array.blit this_overlap 0 overlap 0 (Array.length this_overlap));

      if !sub_length = 0 then add `Remove olds @@ add `Add news out
      else
        let old_idx, new_idx = !sub_range and length = !sub_length in
        let out =
          (* Right*)
          diff_
            (Slice.slice ~from:(old_idx + length)
               ~length:(Slice.length olds - old_idx - length)
               olds)
            (Slice.slice ~from:(new_idx + length)
               ~length:(Slice.length news - new_idx - length)
               news)
            out
        in
        let out = add `Same (Slice.slice ~from:new_idx ~length news) out in
        let out =
          (* Left *)
          diff_
            (Slice.slice ~from:0 ~length:old_idx olds)
            (Slice.slice ~from:0 ~length:new_idx news)
            out
        in
        out

let diff x y = diff_ (Slice.of_list x) (Slice.of_list y) []
