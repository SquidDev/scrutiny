(lang dune 3.3)
(implicit_transitive_deps false)
(generate_opam_files true)
(using mdx 0.2)

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
    (ocaml (>= 4.14))

    ;; Build-time
    (ocamlformat :dev)
    (ocaml-lsp-server :dev)

    ; Core
    cmdliner
    (containers (>= 3.6))
    digestif
    fmt
    fpath
    httpaf-lwt-unix
    jingoo
    logs
    lwt
    lwt_ppx
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
    (alcotest-lwt :with-test)
    ))
