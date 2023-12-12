type exn +=
  | Rpc_exception of string  (** An exception occurred in the underlying RPC protocol. *)
  | Remote_exn of string * string
        (** An error occurred on the remote method call. This contains the error message and
            backtrace. *)

module type VALUE = sig
  type t

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Method : sig
  (** {2 Values}

      In order to use a particular type as an argument or return value, we need to define how to
      encode and decode it. We define the class of serialisable values and provide values for
      several built-in type. *)

  (** A value which can be sent across the wire. *)
  type 'a value = (module VALUE with type t = 'a)

  (** Define a new value type from a decoding and encoding function. *)
  val value : (Yojson.Safe.t -> 'a) -> ('a -> Yojson.Safe.t) -> 'a value

  val unit : unit value
  val int : int value
  val bool : bool value
  val string : string value
  val option : 'a value -> 'a option value
  val result : 'o value -> 'e value -> ('o, 'e) result value
  val pair : 'a value -> 'b value -> ('a * 'b) value

  (** {2 Functions}

      A remote function is composed of a (unique) name, and a function signature. The signature is
      built using {!returning} and [@->], along with the {!value}s it consumes and returns. *)

  (** The signature of a method. This contains the function type (['s]) and its usage (['u]). *)
  type ('s, 'u) signature

  (** This function returns a value. *)
  val returning : 'a value -> ('a, [ `Call ]) signature

  (** Used in place of {!returning} to indicate that this function does not return a value {i and}
      we should not wait for it to complete on the client.*)
  val notify : (unit, [ `Notify ]) signature

  val ( @-> ) : 'a value -> ('b, 'u) signature -> ('a -> 'b, 'u) signature

  (** A concrete method created by {!make}. *)
  type ('s, 'u) t

  (** Create a method from a unique name and a signature. *)
  val make : string -> ('a, 'u) signature -> ('a, 'u) t

  (** A handler for a method. This is just a dependent pair of a method and its instance. *)
  type handle

  (** Create a method handler from a method and function. *)
  val handle : ('a -> 'b, 'u) t -> ('a -> 'b) -> handle
end

(** The interface to make remote calls over *)
type t

(** Create a new RPC instance from an input and output channel. *)
val create : sw:Eio.Switch.t -> Method.handle list -> Eio.Buf_read.t -> _ Eio.Flow.sink -> t

(** Make a remote call and wait for the result. *)
val call : t -> ('a, [ `Call ]) Method.t -> 'a

(** Make a remote call without waiting for the result. *)
val notify : t -> ('a, [ `Notify ]) Method.t -> 'a

(** Create a log reporter which forwards messages over the network.

    This only forwards basic log information (src, level, message) and does not include
    {!Logs.Tag.t
    log tags} or other structured data. *)
val log_forwarder : t -> Logs.reporter

(** Wait for the input to this RPC server to be closed, meaning it can no longer receive any
    messages. *)
val await_closed : t -> unit
