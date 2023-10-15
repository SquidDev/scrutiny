open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module type BasicValue = sig
  type t

  val digest : t -> string
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

module type EdgeOptions = sig
  type t

  val default : t
  val union : t -> t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

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
  let hash = String.hash
  let equal = String.equal
end

module Path = struct
  type t = Fpath.t

  let yojson_of_t x = [%yojson_of: string] (Fpath.to_string x)
  let t_of_yojson x = Fpath.v ([%of_yojson: string] x)
  let digest = Fpath.to_string
  let hash x = Fpath.to_string x |> String.hash
  let equal = Fpath.equal
end
