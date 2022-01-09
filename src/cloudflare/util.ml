module Json = struct
  type t = Yojson.Safe.t

  let t_of_yojson = Fun.id
  and yojson_of_t = Fun.id
end
