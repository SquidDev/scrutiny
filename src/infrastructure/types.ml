module Unit = struct
  type t = unit [@@deriving yojson]

  let digest () = ""

  let default = ()

  let union () () = ()

  let hash () = Hashtbl.hash ()

  let equal () () = true
end

module String = struct
  type t = string [@@deriving yojson]

  let digest x = x

  let hash = CCString.hash

  let equal = String.equal
end

module Path = struct
  type t = Fpath.t

  let yojson_of_t x = [%yojson_of: string] (Fpath.to_string x)

  let t_of_yojson x = Fpath.v ([%of_yojson: string] x)

  let digest = Fpath.to_string

  let hash x = Fpath.to_string x |> CCString.hash

  let equal = Fpath.equal
end
