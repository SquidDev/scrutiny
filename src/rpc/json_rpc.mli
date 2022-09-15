exception Rpc_exception of string
exception Remote_exn of string * string

type error_code =
  | ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | OCaml_exception

(** A JSON RPC error *)
type error = {
  code : error_code;
  message : string;
  data : Yojson.Safe.t option;
}

(** Create an exception from an error. *)
val exn_of_error : error -> exn

(** A message to send across the channel. *)
type message =
  | Call of {
      id : int option;
      method_ : string;
      params : Yojson.Safe.t list;
    }
  | Result of {
      id : int;
      result : Yojson.Safe.t;
    }
  | Error of {
      id : int option;
      error : error;
    }

val message_of_yojson : Yojson.Safe.t -> message
val yojson_of_message : message -> Yojson.Safe.t

(** Create a message from an error. *)
val error : ?id:int -> ?data:Yojson.Safe.t -> error_code -> string -> message
