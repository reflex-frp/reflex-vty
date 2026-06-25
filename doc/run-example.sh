#!/usr/bin/env bash
# Launch the reflex-vty `example` app inside the project's nix-shell so it gets
# the correct vty/terminfo/locale environment. Invoked from doc/demo.tape while
# VHS records the session. The example must already be built (record-demo.sh
# pre-builds it) so `cabal run -v0` is effectively instant.
set -euo pipefail
cd "$(dirname "$0")/.."
exec nix-shell --run 'cabal run -v0 example'
