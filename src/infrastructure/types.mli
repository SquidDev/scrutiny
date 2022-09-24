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

module Unit : sig
  include BasicValue with type t = unit
  include Hashtbl.HashedType with type t := t
  include EdgeOptions with type t := t
end

module String : sig
  include BasicValue with type t = string
  include Hashtbl.HashedType with type t := t
end

module Path : sig
  include BasicValue with type t = Fpath.t
  include Hashtbl.HashedType with type t := t
end
