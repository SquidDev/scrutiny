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

  let to_seq tag { array; start; length } =
    let rec aux i () =
      if i < length then
        let x = array.(start + i) in
        Seq.Cons ((tag, x), aux (i + 1))
      else Seq.Nil
    in
    aux 0

  let iteri f { array; start; length } =
    for i = 0 to length - 1 do
      f i array.(start + i)
    done
end

let rec diff_ olds news =
  match (Slice.length olds, Slice.length news) with
  | 0, 0 -> Seq.empty
  | 0, _ -> Slice.to_seq `Add news
  | _, 0 -> Slice.to_seq `Remove olds
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

      if !sub_length = 0 then Seq.append (Slice.to_seq `Remove olds) (Slice.to_seq `Add news)
      else
        let old_idx, new_idx = !sub_range and length = !sub_length in
        let left =
          diff_
            (Slice.slice ~from:0 ~length:old_idx olds)
            (Slice.slice ~from:0 ~length:new_idx news)
        and right =
          diff_
            (Slice.slice ~from:(old_idx + length)
               ~length:(Slice.length olds - old_idx - length)
               olds)
            (Slice.slice ~from:(new_idx + length)
               ~length:(Slice.length news - new_idx - length)
               news)
        in
        List.to_seq [ left; Slice.to_seq `Same (Slice.slice ~from:new_idx ~length news); right ]
        |> Seq.flat_map Fun.id

let diff x y = diff_ (Slice.of_list x) (Slice.of_list y) |> List.of_seq
