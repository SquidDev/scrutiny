(lang dune 2.7)
(implicit_transitive_deps false)
(generate_opam_files true)

(name scrutiny)
(version 0.1.0)
(authors "Jonathan Coates")
(license BSD-3-Clause)
(source (github squiddev/scrutiny))
(maintainers "Jonathan Coates <git@squiddev.cc>")

(package
  (name scrutiny)
  (synopsis "Bits and bobs for wrangling my server. ")
  (description "A home grown infrastructure management and monitoring toolkit, because I'm silly.")
  (depends
    (ocaml (>= 4.08))

    ;; Build-time
    dune ; While a build-tool, dune shouldn't be build-time only

    (ocamlformat :dev)
    (ocaml-lsp-server :dev)

    ; Core
    cmdliner
    cohttp
    cohttp-lwt
    cohttp-lwt-unix
    (containers (>= 3.0.1))
    ctypes
    digestif
    fmt
    fpath
    httpaf
    httpaf-lwt-unix
    integers
    jingoo
    logs
    lwt
    lwt_ppx
    ppx_yojson_conv
    ppx_yojson_conv_lib
    prometheus
    re
    react
    result
    sqlite3
    tls
    uri
    yojson

    ; vendor/obus
    lwt_log
    lwt_react
    menhir
    ppxlib
    xmlm
    ))
