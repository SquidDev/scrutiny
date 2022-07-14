#!/usr/bin/env sh
# Wrapper script which looks like SSH but runs inside a podman container instead.

set -u

CONTAINER="$1"
shift
exec podman exec -i "$CONTAINER" "$@"

# It's useful to run the tunnel under valgrind to debug weird leaks.
# exec podman exec -i "$CONTAINER" valgrind --show-leak-kinds=all --track-origins=yes --leak-check=full "$@"
