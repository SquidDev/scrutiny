open Scrutiny_infrastructure

type file_state = {
  user : User.t;
  group : User.t;
  perms : Unix.file_perm;
  contents : string;
  make_dirs : bool;
}

val file_resource : (Fpath.t, file_state, unit) Resource.t
