type id = string [@@deriving yojson]

let pp_id = Format.pp_print_string

type t =
  { id : id;
    type_ : string; [@key "type"]
    name : string;
    content : string;
    proxiable : bool;
    proxied : bool;
    ttl : int;
    locked : bool;
    zone_id : Api_zone.id;
    zone_name : string;
    created_on : string;
    modified_on : string;
    data : Util.Json.t option; [@yojson.option]
    priority : int option [@yojson.option]
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

let pp_json fmt = function
  | None -> ()
  | Some x -> Format.fprintf fmt " %s" (Yojson.Safe.to_string x)

let pp_short fmt r = Format.fprintf fmt "%5s %s=%S%a" r.type_ r.name r.content pp_json r.data

let pp fmt r = Format.fprintf fmt "{%s} %a" r.id pp_short r

let list ~auth ~zone =
  Request.call ~auth
    ~query:[ ("per_page", [ "100" ]) ]
    ~parse:(list_of_yojson t_of_yojson) `GET
    (Printf.sprintf "zones/%s/dns_records" zone)

let add_opt name value xs : (string * Yojson.Safe.t) list =
  match value with
  | None -> xs
  | Some value -> (name, value) :: xs

let body_common ~type_ ~name ~content ?proxied ?(ttl = 1) () : (string * Yojson.Safe.t) list =
  [ ("name", `String name);
    ("type", `String type_);
    ("content", `String content);
    ("ttl", `Int ttl)
  ]
  |> add_opt "proxied" (Option.map (fun x -> `Bool x) proxied)

let add ~auth ~zone ~type_ ~name ~content ?proxied ?ttl ?priority ?data () =
  let body =
    body_common ~type_ ~name ~content ?proxied ?ttl ()
    |> add_opt "priority" (Option.map (fun x -> `Int x) priority)
    |> add_opt "data" data
  in
  Request.call ~auth
    ~body:(Yojson.Safe.to_string (`Assoc body))
    ~parse:t_of_yojson `POST
    (Printf.sprintf "zones/%s/dns_records" zone)

let update ~auth ~zone ~type_ ~name ~content ?proxied ?ttl id =
  let body = body_common ~type_ ~name ~content ?proxied ?ttl () in
  Request.call ~auth
    ~body:(Yojson.Safe.to_string (`Assoc body))
    ~parse:t_of_yojson `PUT
    (Printf.sprintf "zones/%s/dns_records/%s" zone id)

let delete ~auth ~zone id =
  Request.call ~auth ~parse:(fun _ -> ()) `DELETE (Printf.sprintf "zones/%s/dns_records/%s" zone id)
