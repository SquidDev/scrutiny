(lang dune 3.7)
(implicit_transitive_deps false)
(generate_opam_files true)
(using mdx 0.3)

(name scrutiny)
(version 0.1.0)
(authors "Jonathan Coates")
(license BSD-3-Clause)
(source (github squiddev/scrutiny))
(maintainers "Jonathan Coates <git@squiddev.cc>")

(package
  (name scrutiny)
  (synopsis "Bits and bobs for wrangling my server")
  (description "A home grown infrastructure management and monitoring toolkit, because I'm silly.")
  (depends
    (ocaml (>= 5.0))

    ;; Build-time
    (ocamlformat :dev)
    (ocaml-lsp-server :dev)

    ; Core
    base-threads
    base-unix
    cmdliner
    (cohttp (>= 6.0))
    (cohttp-eio (>= 6.0))
    (containers (>= 3.6))
    digestif
    (eio (>= 0.10))
    eio_main
    fmt
    fpath
    jingoo
    logs
    lwt-dllist
    ocurl
    ppx_yojson_conv
    ppx_yojson_conv_lib
    (progress (>= 0.2.0))
    (prometheus (= 1.1))
    re
    result
    uri
    yojson

    ; Test
    (mdx :with-test)
    (alcotest :with-test)
    ))
