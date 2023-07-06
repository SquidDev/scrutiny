open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type exn += Rpc_exception of string | Remote_exn of string * string

let () =
  Printexc.register_printer (function
    | Rpc_exception msg -> Some ("Rpc_exception: " ^ msg)
    | Remote_exn (msg, bt) -> Some (msg ^ " at:\n" ^ bt)
    | _ -> None)

type yojson = Yojson.Safe.t

let yojson_of_yojson x = x

type error_code =
  (* Builtin errors. *)
  | ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  (* Custom errors. *)
  | OCaml_exception

let error_code_of_yojson : Yojson.Safe.t -> error_code = function
  | `Int -32700 -> ParseError
  | `Int -32600 -> InvalidRequest
  | `Int -32601 -> MethodNotFound
  | `Int -32602 -> InvalidParams
  | `Int -32603 -> InternalError
  | `Int 1 -> OCaml_exception
  | _ -> raise (Yojson.Json_error "Unknown error code")

let yojson_of_error_code : error_code -> Yojson.Safe.t = function
  | ParseError -> `Int (-32700)
  | InvalidRequest -> `Int (-32600)
  | MethodNotFound -> `Int (-32601)
  | InvalidParams -> `Int (-32602)
  | InternalError -> `Int (-32603)
  | OCaml_exception -> `Int 1

type error = {
  code : error_code;
  message : string;
  data : yojson option; [@yojson.option]
}
[@@deriving yojson]

(** Convert an error to an exception. *)
let exn_of_error = function
  | { code = OCaml_exception; message; data = Some (`String bt) } -> Remote_exn (message, bt)
  | { message; _ } -> Rpc_exception message

type message =
  | Call of {
      id : int option;
      method_ : string;
      params : yojson list;
    }
  | Result of {
      id : int;
      result : yojson;
    }
  | Error of {
      id : int option;
      error : error;
    }

(** Construct an error message. *)
let error ?id ?data code message : message = Error { id; error = { code; message; data } }

let message_of_yojson : Yojson.Safe.t -> message = function
  | `Assoc fields -> (
      let has_jsonrpc = ref false in
      let meth = ref None in
      let params = ref None in
      let error = ref None in
      let result = ref None in
      let id = ref None in

      List.iter
        (fun ((k, v) : string * Yojson.Safe.t) ->
          match (k, v) with
          | "jsonrpc", `String "2.0" -> has_jsonrpc := true
          | "method", `String meth' -> meth := Some meth'
          | "params", `List params' -> params := Some params'
          | "error", error' -> error := Some (error_of_yojson error')
          | "result", result' -> result := Some result'
          | "id", `Int id' -> id := Some id'
          | "id", `Null -> id := None
          | k, _ -> raise (Yojson.Json_error ("Unknown/invalid key " ^ k)))
        fields;

      if not !has_jsonrpc then raise (Yojson.Json_error "Not a JSON-RPC message");

      match (!meth, !params, !result, !error, !id) with
      | Some method_, Some params, None, None, id -> Call { id; method_; params }
      | None, None, Some result, None, Some id -> Result { id; result }
      | None, None, None, Some error, id -> Error { id; error }
      | _ -> raise (Yojson.Json_error "Incomplete method call"))
  | _ -> failwith "Expected field"

let version = ("jsonrpc", `String "2.0")

let yojson_of_message : message -> Yojson.Safe.t = function
  | Call { id = None; method_; params } ->
      `Assoc [ version; ("method", `String method_); ("params", `List params) ]
  | Call { id = Some id; method_; params } ->
      `Assoc [ version; ("id", `Int id); ("method", `String method_); ("params", `List params) ]
  | Result { id; result } -> `Assoc [ version; ("id", `Int id); ("result", result) ]
  | Error { id = None; error } ->
      `Assoc [ version; ("id", `Null); ("error", yojson_of_error error) ]
  | Error { id = Some id; error } ->
      `Assoc [ version; ("id", `Int id); ("error", yojson_of_error error) ]
