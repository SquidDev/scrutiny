open Core

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
