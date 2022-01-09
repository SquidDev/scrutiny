open Lwt.Infix

type id = string [@@deriving yojson]

let pp_id = Format.pp_print_string

type t = {
  id : id;
  name : string;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

let find ~auth name =
  Request.call ~auth
    ~query:[ ("match", [ "any" ]); ("name", [ name ]) ]
    ~parse:(list_of_yojson t_of_yojson) `GET "zones"
  >|= function
  | Some ({ id; _ } :: _) -> Some id
  | _ -> None
