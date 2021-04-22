(lang dune 2.7)
(using menhir 2.0)
(implicit_transitive_deps false)
(generate_opam_files true)

(name scrutiny)
(version 0.1.0)
(authors "Jonathan Coates")
(license BSD-3-Clause)
(source (github squiddev/status.squiddev.cc))
(maintainers "Jonathan Coates <git@squiddev.cc>")

(package
  (name scrutiny)
  (synopsis "Checks how well squid is doing")
  (description "A home grown infrastructure monitoring toolkit, because I'm silly.")
  (depends
    (ocaml (>= 4.08))

    ;; Build-time
    dune ; While a build-tool, dune shouldn't be build-time only

    (ocamlformat :dev)
    (ocaml-lsp-server :dev)

    ; Core
    cmdliner
    (containers (>= 3.0.1))
    ctypes
    fmt
    fpath
    httpaf
    httpaf-lwt-unix
    logs
    lwt
    obus
    ppx_yojson_conv
    prometheus
    re
    ))
