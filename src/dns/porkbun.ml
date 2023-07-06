open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Types

type auth = {
  api_key : string;
  secret : string;
}

type client = {
  client : Request.client;
  auth : auth;
}

let create ~client ~auth = { client; auth }

type Zone.id += Porkbun_zone of string
type Dns_record.id += Porkbun_record of string

let () =
  Zone.register_id_printer (fun out id ->
      match id with
      | Porkbun_zone domain -> Format.pp_print_string out domain; true
      | _ -> false);
  Dns_record.register_id_printer (fun out id ->
      match id with
      | Porkbun_record domain -> Format.pp_print_string out domain; true
      | _ -> false)

let get_zone = function
  | Porkbun_zone zone -> zone
  | _ -> invalid_arg "Not a Porkbun domain"

let get_record = function
  | Porkbun_record record -> record
  | _ -> invalid_arg "Not a Porkbun record"

let decode = function
  | Error message -> Error message
  | Ok body -> (
    match Yojson.Safe.from_string body with
    | `Assoc body -> (
      match List.assoc_opt "status" body with
      | Some (`String "SUCCESS") -> Ok body
      | Some (`String "ERROR") ->
          let message =
            match List.assoc_opt "message" body with
            | Some (`String message) -> message
            | _ -> "Unknown reason"
          in
          Error message
      | _ -> Error "Could not determine status")
    | _ -> Error (Printf.sprintf "Unknown JSON body %S" body)
    | exception Yojson.Json_error e -> Error (Printf.sprintf "Error decoding JSON %S => %s" body e))

let make_request ~client:{ client; auth } (fields : (string * Yojson.Safe.t) list) path =
  let fields =
    ("secretapikey", `String auth.secret) :: ("apikey", `String auth.api_key) :: fields
  in
  let body = `Assoc fields |> Yojson.Safe.to_string in
  let uri = Uri.make ~scheme:"https" ~host:"porkbun.com" ~path:("/api/json/v3/dns" ^ path) () in
  let result = Request.call ~client ~headers:[] (POST body) uri in
  decode result

let find_zone ~client:_ zone = Ok (Porkbun_zone zone)

type record_id = string

let record_id_of_yojson : Yojson.Safe.t -> record_id = function
  | `String x -> x
  | `Int x -> string_of_int x
  | j -> raise (Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure "Not an int/string", j))

type dns_record = {
  id : record_id;
  name : string;
  type_ : string; [@key "type"]
  content : string;
  ttl : string;
  prio : string option;
}
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

let convert_record zone_id (record : dns_record) : Dns_record.t =
  let priority =
    match record.prio with
    | None -> None
    | Some "0" when record.type_ <> "MX" -> None
    | Some x -> Some (int_of_string x)
  in
  {
    id = Porkbun_record record.id;
    zone_id = Porkbun_zone zone_id;
    type_ = record.type_;
    name = record.name;
    content = record.content;
    ttl = int_of_string record.ttl;
    priority;
  }

let list_records ~client ~zone =
  let zone = get_zone zone in
  match make_request ~client [] ("/retrieve/" ^ zone) with
  | Error e -> Error e
  | Ok fields ->
      List.assoc "records" fields
      |> list_of_yojson dns_record_of_yojson
      |> List.filter (fun x -> x.type_ <> "NS")
      |> List.map (convert_record zone)
      |> Result.ok

(** Get the name of the domain, stripping off the root zone. *)
let get_name ~zone name =
  if zone = name then ""
  else if String.ends_with ~suffix:("." ^ zone) name then
    String.sub name 0 (String.length name - String.length zone - 1)
  else invalid_arg (Printf.sprintf "%s does not belong to %s" name zone)

(** Construct the JSON object required for the create/update request. *)
let make_fields ~zone ~type_ ~name ~content ?ttl ?priority () =
  let opt_field name f x xs : (string * Yojson.Safe.t) list =
    match x with
    | None -> xs
    | Some x -> (name, `String (f x)) :: xs
  in
  let fields =
    [
      ("name", `String (get_name ~zone name)); ("type", `String type_); ("content", `String content);
    ]
  in
  let fields = opt_field "ttl" string_of_int ttl fields in
  let fields = opt_field "prio" string_of_int priority fields in
  fields

let add ~client ~zone ~type_ ~name ~content ?ttl ?priority () =
  let zone = get_zone zone in
  let fields = make_fields ~zone ~type_ ~name ~content ?ttl ?priority () in
  match make_request ~client fields ("/create/" ^ zone) with
  | Error e -> Error e
  | Ok fields ->
      let id = List.assoc "id" fields |> record_id_of_yojson in
      Ok (Porkbun_record id)

let update ~client ~zone ~type_ ~name ~content ?ttl ?priority id =
  let zone = get_zone zone and id = get_record id in
  let fields = make_fields ~zone ~type_ ~name ~content ?ttl ?priority () in
  match make_request ~client fields (Printf.sprintf "/edit/%s/%s" zone id) with
  | Error e -> Error e
  | Ok _ -> Ok ()

let delete ~client ~zone id =
  let zone = get_zone zone and id = get_record id in
  match make_request ~client [] (Printf.sprintf "/delete/%s/%s" zone id) with
  | Error e -> Error e
  | Ok _ -> Ok ()
