(** Cheap and lazy heterogeneous hashmap. *)

(** A proof that two variables of different types are equal, and thus their types are equal. *)
module Eq : sig
  type ('a, 'b) t =
    | Eq : ('a, 'a) t
    | Ineq : ('a, 'b) t

  (** A function which, given two values of different types, yields a proof whether the values (and
      types) are equal. *)
  type ('a, 'b) is_eq = 'a -> 'b -> ('a, 'b) t

  (** Check two variables are equal according to their underlying memory location. *)
  val by_ref : ('a, 'b) is_eq

  (** Convert this equality to a boolean. *)
  val to_bool : ('a, 'b) t -> bool
end

module type HASH = sig
  type 'a t

  val hash : 'a t -> int
  val equal : 'a t -> 'b t -> ('a, 'b) Eq.t
end

module type VALUE = sig
  type 'a t
end

module Make (K : HASH) (V : VALUE) : sig
  type t
  type packed = Packed : 'a K.t * 'a V.t -> packed

  val create : int -> t
  val set : t -> 'a K.t -> 'b V.t -> unit
  val get : t -> 'a K.t -> 'a V.t option
  val get_exn : t -> 'a K.t -> 'a V.t
  val mem : t -> 'a K.t -> bool
  val length : t -> int
  val to_seq : t -> packed Seq.t
end
