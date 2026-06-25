#!/usr/bin/env bash
# Run one of the gallery demos inside the project nix-shell (so it gets the
# correct vty/terminfo/locale environment). Invoked from the per-example VHS
# tapes; the executable must already be built (record-example.sh builds it).
cd "$(dirname "$0")/.."
exec nix-shell --run "cabal run -v0 examples -- ${1:-spinner}"
