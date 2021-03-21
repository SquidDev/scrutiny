# Scrutiny
Scrutiny is a small collection of OCaml utilities for keeping an eye on and
maintaining my server. I wouldn't recommend it to other people - there's better
solutions out there, but I enjoy reinventing the wheel far too much to use them.


## Components
Scrutiny is formed of lots of small libraries. There's not any coherency here - 
it's just formed of things I found useful!

### cloudflare
A wrapper for Cloudflare's API, allowing you to create, delete and update DNS
records. This also provides a module to "sync" DNS records, adding/removing
records to match a local specification.

### systemd
 - Partial bindings to `sd-journal.h`. This was originally written for a log
   storage system, but I ended up using [Loki][loki] instead.
   
   This also supports writing to the journal, though this is not (currently)
   used anyway.

 - Very incomplete bindings to the systemd DBus interface. This is largely
   intended for `systemd_exporter`, so can't do much.

### systemd_exporter
A Prometheus metrics exporter for systemd units, similar to [systemd_exporter]
and [node_exporter]. This finds all units and provides basic stats (CPU, memory)
about running services.

While it's less detailed than [systemd_exporter], it supports listening to
multiple DBus addresses, meaning it can monitor both system and user services.

[loki]: https://grafana.com/oss/loki/ "Grafana Loki"
[systemd_exporter]: https://github.com/povilasv/systemd_exporter/ "systemd_exporter on GitHub"
[node_exporter]: https://github.com/prometheus/node_exporter "node_exporter on GitHub"
