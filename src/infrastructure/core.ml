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
  val apply : env:Eio.Stdenv.t -> Key.t -> Value.t -> EdgeOptions.t -> (change, string) result Lwt.t
end

module Resource = struct
  type ('key, 'value, 'options) t =
    (module Resource
       with type Key.t = 'key
        and type Value.t = 'value
        and type EdgeOptions.t = 'options)

  type boxed = Boxed : ('key, 'value, 'options) t -> boxed [@@unboxed]

  let resources = ref SMap.empty

  let make (type key value options) (resource : (key, value, options) t) =
    let module R = (val resource) in
    let r = !resources in
    if SMap.mem R.id r then invalid_arg "Duplicate module id";
    resources := SMap.add R.id (Boxed resource) r;
    resource
end

module Machine = struct
  type t =
    | Local
    | Remote of Remote.t

  let equal x y =
    match (x, y) with
    | Local, Local -> true
    | Remote x, Remote y -> x == y
    | Local, Remote _ | Remote _, Local -> false
end

module Concrete_resource = struct
  (** A resource instantiated in a specific context.

      In the future I'd like to remove user/machine and make them part of the key. *)
  type ('key, 'value, 'options) t = {
    resource : ('key, 'value, 'options) Resource.t;
    key : 'key;
    user : user;
    machine : Machine.t;
  }

  let hash (type key value options) ({ resource; key; _ } : (key, value, options) t) =
    let module R = (val resource) in
    R.Key.hash key

  let equal (type k1 v1 o1 k2 v2 o2) : ((k1, v1, o1) t, (k2, v2, o2) t) Het_map.Eq.is_eq =
   fun l r ->
    match Het_map.Eq.by_ref l.resource r.resource with
    | Ineq -> Ineq
    | Eq when l.user <> r.user || l.machine != r.machine -> Ineq
    | Eq ->
        let module R = (val l.resource) in
        if R.Key.equal l.key r.key then Eq else Ineq
end

type context = {
  machine : Machine.t;
  user : user;
}

type ('var, 'ctx) var = {
  id : int;
  name : string option;
  pos : (string * int * int * int) option;
  get_context : context -> 'ctx;
  equal : 'ctx -> 'ctx -> bool;
}

module Concrete_var = struct
  type 'a t =
    | CVar : {
        var : ('a, 'ctx) var;
        context : 'ctx;
      }
        -> 'a t

  let hash (CVar { var; _ }) = var.id

  let equal (type a b) : a t -> b t -> (a, b) Het_map.Eq.t =
   fun (CVar v1) (CVar v2) ->
    match Het_map.Eq.by_ref v1.var v2.var with
    | Eq -> if v1.var.equal v1.context v2.context then Eq else Ineq
    | Ineq -> Ineq

  let pp out (CVar { var; _ }) =
    match var with
    | { name = Some name; _ } -> Format.fprintf out "Var %s" name
    | { pos = Some (file, line, _, _); _ } -> Format.fprintf out "Var defined at %s:%d" file line
    | _ -> Format.fprintf out "Anonymous var"

  let create var context = CVar { var; context = var.get_context context }
end

(** The user-facing key fed into an action. *)
type ('result, 'kind) key =
  | Resource : ('key, 'value, 'options) Concrete_resource.t -> (unit, [ `Resource ]) key
  | Var : ('value, 'ctx) var -> ('value list, [ `Var ]) key

module Var = struct
  let id = Atomic.make 0

  let make ?name ?__POS__ ~get_context ~equal () =
    let id = Atomic.fetch_and_add id 1 in
    Var { id; name; pos = __POS__; get_context; equal }

  let make_global ?name ?__POS__ () =
    make ?name ?__POS__ ~get_context:(fun _ -> ()) ~equal:Unit.equal ()

  let make_per_machine ?name ?__POS__ () =
    make ?name ?__POS__ ~get_context:(fun _ -> ()) ~equal:Unit.equal ()
end

module Concrete_key = struct
  (** An key instantiated for a specific context.

      This takes two type parameters ['result] (the return value of this key) and ['extra] (ensures
      the builder is consistent with the key). These are packed into a single type parameter via a
      tuple to make it easier to work with {!Het_map}. *)
  type 'a t =
    | Resource :
        ('key, 'value, 'options) Concrete_resource.t
        -> (unit * [ `Resource of 'value * 'options ]) t
    | Var : 'a Concrete_var.t -> ('a list * [ `Var ]) t

  let hash (type a) : a t -> int = function
    | Resource r -> Concrete_resource.hash r
    | Var v -> Concrete_var.hash v

  let equal (type a b) (l : a t) (r : b t) : (a, b) Het_map.Eq.t =
    match (l, r) with
    | Resource l, Resource r -> (
      match Concrete_resource.equal l r with
      | Eq -> Eq
      | Ineq -> Ineq)
    | Var l, Var r -> (
      match Concrete_var.equal l r with
      | Eq -> Eq
      | Ineq -> Ineq)
    | Resource _, Var _ | Var _, Resource _ -> Ineq

  type boxed = BKey : 'result t -> boxed [@@unboxed]

  (** A key which just exposes the result. Convenient for exposing the extra data as an existential. *)
  type 'result with_res = RKey : ('result * 'data) t -> 'result with_res [@@unboxed]

  let create (type result kind) context : (result, kind) key -> result with_res = function
    | Resource r -> RKey (Resource r)
    | Var v -> RKey (Var (Concrete_var.create v context))

  let pp (type a) out : a t -> unit = function
    | Resource { resource = (module R); key; _ } -> R.pp out key
    | Var v -> Concrete_var.pp out v

  module Boxed = struct
    type t = boxed

    let hash (BKey b) = hash b
    let equal (BKey l) (BKey r) = equal l r |> Het_map.Eq.to_bool
    let pp out (BKey b) = pp out b
  end
end

type ('value, 'options) action =
  | Pure : 'value -> ('value, 'options) action
  | Need : 'options * ('result * 'extra) Concrete_key.t -> ('result, 'options) action
  | Bind : ('a, 'options) action * ('a -> 'b) -> ('b, 'options) action
  | Pair : ('a, 'options) action * ('b, 'options) action -> ('a * 'b, 'options) action

type ('value, 'options) action_deps = {
  edges : Concrete_key.boxed list;
  term : ('value, 'options) action;
}

module Action = struct
  type 'options env = {
    options : (module EdgeOptions with type t = 'options);
    context : context;
  }

  type ('value, 'options) t = 'options env -> ('value, 'options) action_deps

  let ( let+ ) (x : ('a, 'options) t) (f : 'a -> 'b) options =
    let x = x options in
    { x with term = Bind (x.term, f) }

  let ( and+ ) x y m =
    let x = x m and y = y m in
    { edges = x.edges @ y.edges; term = Pair (x.term, y.term) }

  let need (type options) ?options key (env : options env) : ('value, options) action_deps =
    let module E = (val env.options) in
    let (RKey key) = Concrete_key.create env.context key in
    { edges = [ BKey key ]; term = Need (Option.value ~default:E.default options, key) }

  let value x _ = { edges = []; term = Pure x }
end

(** Information needed to build a key. This has an identical structure to {!Concrete_key}. *)
module Key_builder = struct
  type 'a t =
    | Resource :
        ('value Lwt.t, 'options) action_deps
        -> (unit * [ `Resource of 'value * 'options ]) t
    | Var : ('value Lwt.t list, unit) action_deps -> ('value list * [ `Var ]) t

  let dependencies (type a) : a t -> _ = function
    | Resource r -> r.edges
    | Var r -> r.edges
end

module Builder_map = Het_map.Make (Concrete_key) (Key_builder)

module Rules = struct
  type rules = Builder_map.t

  type env = {
    context : context;
    rules : rules;
  }

  type ('ctx, 'a) t = env -> 'a

  let ( let* ) (x : ('ctx, 'a) t) (f : 'a -> ('ctx, 'b) t) env : 'b =
    let x = x env in
    f x env

  let pure x _ = x

  let resource (type key value options) (resource : (key, value, options) Resource.t) key
      (value : unit -> (value Lwt.t, options) Action.t) (env : env) : _ =
    let module R = (val resource) in
    let value = value () { options = (module R.EdgeOptions); context = env.context } in
    let key =
      { Concrete_resource.resource; key; user = env.context.user; machine = env.context.machine }
    in
    Builder_map.set env.rules (Resource key) (Resource value);
    Resource key

  let extend (type value) (Var key : (value list, [ `Var ]) key)
      (value : unit -> (value Lwt.t, unit) Action.t) (env : env) : unit =
    let value = value () { options = (module Types.Unit); context = env.context } in
    let key = Concrete_key.Var (Concrete_var.create key env.context) in
    let values =
      match Builder_map.get env.rules key with
      | None -> Action.value [] ()
      | Some (Var v) -> v
    in
    let values : _ action_deps =
      {
        edges = value.edges @ values.edges;
        term = Bind (Pair (value.term, values.term), fun (new_def, defs) -> new_def :: defs);
      }
    in
    Builder_map.set env.rules key (Var values)

  let with_context lift f env = f () { env with context = lift env.context }

  let with_user user (f : unit -> ([ `User ], 'a) t) env : 'a =
    with_context (fun c -> { c with user = `Name user }) f env

  let with_user_uid user (f : unit -> ([ `User ], 'a) t) env : 'a =
    with_context (fun c -> { c with user = `Id user }) f env

  let with_remote remote (f : unit -> ([ `Remote ], 'a) t) env : 'a =
    with_context (fun c -> { c with machine = Remote remote }) f env
end
