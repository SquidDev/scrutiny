module SMap = Map.Make (String)

type user =
  [ `Current
  | `Id of int
  | `Name of string
  ]

module type BasicValue = sig
  type t

  val digest : t -> string
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

module Remote = struct
  type t = {
    host : string;
    tunnel_path : string;
    sudo_pw : string option;
  }

  let make ?sudo_pw ?(tunnel_path = "scrutiny-infra-tunnel") host = { host; tunnel_path; sudo_pw }
end

type change =
  | Correct
  | NeedsChange of {
      diff : Scrutiny_diff.t;
      apply : unit -> (unit, string) result Lwt.t;
    }

module type EdgeOptions = sig
  type t

  val default : t
  val union : t -> t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

module type Resource = sig
  val id : string

  module Key : sig
    include BasicValue
    include Hashtbl.HashedType with type t := t
  end

  module EdgeOptions : EdgeOptions
  module Value : BasicValue

  val pp : Key.t Fmt.t
  val apply : Key.t -> Value.t -> EdgeOptions.t -> (change, string) result Lwt.t
end

module Resource = struct
  type ('key, 'value, 'options) t =
    (module Resource
       with type Key.t = 'key
        and type Value.t = 'value
        and type EdgeOptions.t = 'options)

  type boxed = Boxed : ('key, 'value, 'options) t -> boxed

  let resources = ref SMap.empty

  let make (type key value options) (resource : (key, value, options) t) =
    let module R = (val resource) in
    let r = !resources in
    if SMap.mem R.id r then invalid_arg "Duplicate module id";
    resources := SMap.add R.id (Boxed resource) r;
    resource
end

type machine =
  | Local
  | Remote of Remote.t

type ('key, 'value, 'options) resource = {
  resource : ('key, 'value, 'options) Resource.t;
  key : 'key;
  dependencies : boxed_key list;
  value : ('value Lwt.t, 'options) action;
  user : user;
  machine : machine;
}

and ('result, 'kind) key =
  | Resource : ('key, 'value, 'options) resource -> (unit, [ `Resource ]) key

and ('value, 'options) action =
  | Pure : 'value -> ('value, 'options) action
  | Need : 'options * ('result, 'kind) key -> ('result, 'options) action
  | Bind : ('a, 'options) action * ('a -> 'b) -> ('b, 'options) action
  | Pair : ('a, 'options) action * ('b, 'options) action -> ('a * 'b, 'options) action

and boxed_key = BKey : ('result, 'kind) key -> boxed_key

module Action = struct
  type ('value, 'options) main_action = {
    edges : boxed_key list;
    term : ('value, 'options) action;
  }

  type ('value, 'options) t =
    (module EdgeOptions with type t = 'options) -> ('value, 'options) main_action

  let ( let+ ) (x : ('a, 'options) t) (f : 'a -> 'b) options =
    let x = x options in
    { x with term = Bind (x.term, f) }

  let ( and+ ) x y m =
    let x = x m and y = y m in
    { edges = x.edges @ y.edges; term = Pair (x.term, y.term) }

  let need (type options) ?options key (module E : EdgeOptions with type t = options) :
      ('value, options) main_action =
    { edges = [ BKey key ]; term = Need (Option.value ~default:E.default options, key) }

  let value x _ = { edges = []; term = Pure x }
end

module Rules = struct
  type rules = boxed_key list

  type context = {
    rules : rules;
    user : user;
    machine : machine;
  }

  type ('ctx, 'a) t = context -> 'a * rules

  let ( let* ) (x : ('ctx, 'a) t) (f : 'a -> ('ctx, 'b) t) context : 'b * rules =
    let x, rules = x context in
    f x { context with rules }

  let pure x context : 'a * rules = (x, context.rules)

  let resource (type key value options) (resource : (key, value, options) Resource.t) key
      (value : unit -> (value Lwt.t, options) Action.t) (context : context) : _ * rules =
    let module R = (val resource) in
    let value = value () (module R.EdgeOptions) in
    let key =
      Resource
        {
          resource;
          key;
          value = value.term;
          dependencies = value.edges;
          user = context.user;
          machine = context.machine;
        }
    in
    (key, BKey key :: context.rules)

  let with_user user (f : unit -> ([ `User ], 'a) t) context : 'a * rules =
    f () { context with user = `Name user }

  let with_user_uid user (f : unit -> ([ `User ], 'a) t) context : 'a * rules =
    f () { context with user = `Id user }

  let with_remote remote (f : unit -> ([ `Remote ], 'a) t) context : 'a * rules =
    f () { context with machine = Remote remote }
end

module KeyTbl = Hashtbl.Make (struct
  type t = boxed_key

  let equal (BKey (Resource l)) (BKey (Resource r)) = Obj.repr l == Obj.repr r

  let hash (BKey (Resource { resource; key; _ })) =
    let module R = (val resource) in
    R.Key.hash key
end)
