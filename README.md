# Scrutiny
Scrutiny is a small collection of OCaml utilities for maintaining and monitoring
my server. Its main component is a configuration management system, but also
includes some useful libraries and a prometheus exporter for systemd services.

While I use scrutiny to manage my own server, I wouldn't recommend it to other
people - it's still in flux, and there's plenty of better tools out there. I
just enjoy reinventing the wheel far too much.

## `scrutiny-infra`: Another configuration management system
Scrutiny is another configuration management system, designed for pushing config
to a single server (or potentially a _very_ small fleet of servers). It draws
inspiration from systems like [Ansible] and [Salt], while attempting to overcome
some of their shortcomings.

### Example
Rather than using YAML (or worse, YAML and Jinga 2 templates), resources
(anything managed by Scrutiny) are described in OCaml.

For instance, here's how one may configure a basic web server:

```ml
(* Upload a config file to /etc/nginx/nginx.conf, based on the nginx.conf template *)
let* conf =
  template (Fpath.v "/etc/nginx/nginx.conf") ~template:Fpath.(templates / "nginx.conf")
  @@ fun () -> value []
in
let* unit_file = template (Fpath.v "/etc/systemd/system/openresty.service") (* ... *)
in
(* Ensure the "openresty" service is enabled and running. This only runs once our
  config file and systemd service have been updated. If either have changed, the
  openresty service will be restarted or reloaded as appropriate. *)
let* _service =
  service ~name:"openresty" ~scope:`System @@ fun () ->
  let+ () = need ~options:`Reload conf
  and+ () = need ~options:`Restart unit_file in
  service_state ~enabled:true ~running:true ~monitor:5 ()
in
pure ()
```

This is a little confusing, especially for those not used to OCaml's syntax, so
let's highlight a few things here:

 - Resources (something managed by scrutiny, like a file or service) are
   declared by `let*` (within a `Rules.t` monad). Each resource has a name (such
   as a file path) and a function to compute the desired state of this resource
   (such as a file's contents).

 - Resources may declare a set of dependencies using `let+` and `and+`. These
   are created and updated before this resource. For instance, our `openresty`
   service obviously needs its configuration file!

 - The `need` function accepts additional metdata, which is passed to the
   dependent resource when the dependency changes (for instance, reloading
   openresty when a config file changes).

We can then compile this code into an executable and run it (annoying, I know,
but OCaml's compiler is fast). This then SSHes into your server and begins
pushing changes ([@purpleidea would argue that makes us an orchestrator rather
than a config manager][mgmt-info] - the distinction does confuse me).

### Non-features
Scrutiny was very much written to scratch my itch. As a result, it's missing
lots of features that others may find valuable:

 - **YAML or other configuration files:** While it's possible to write your own
   system to load variables from disk, there's no support for it by default - I
   just bake them into the code.

   This does mean you also don't get useful features like inheritance/overrides.

 - **Encrypted/private resources:** Scrutiny reads directly from the filesystem,
   and isn't smart enough to decrypt files. Combined with the above, this may
   mean you're storing secrets in your executable - probably best avoided!

 - **Dynamic resources:** The entire resource graph must be known ahead-of-time.
   While the desired state of a resource may change depending on the server
   state, it's not possible to add/remove resources based on server state.

 - **Stability and support:** Look, it's just me using it!

[ansible]: https://www.ansible.com/ "Ansible is Simple IT Automation"
[salt]: https://saltproject.io/ "Salt Project"
[mgmt-info]: https://purpleidea.com/blog/2016/01/18/next-generation-configuration-mgmt/

