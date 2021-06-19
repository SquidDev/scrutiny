#!/usr/bin/env sh

set -xe

dune build -p scrutiny @install
dune install --root=. --prefix _install
scp -C _install/bin/scrutiny-infra-tunnel autoclave.squiddev.cc:.local/bin/scrutiny-infra-tunnel
