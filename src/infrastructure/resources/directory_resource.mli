open Scrutiny_infrastructure

type dir_state = {
  user : User.t;
  group : User.t;
  perms : Unix.file_perm;
}

val dir_resource : (Fpath.t, dir_state, unit) Resource.t
