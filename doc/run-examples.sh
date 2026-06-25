#!/usr/bin/env bash
# Run one of the gallery demos inside the project nix-shell (so it gets the
# correct vty/terminfo/locale environment). Invoked from the per-example VHS
# tapes; the executable must already be built (record-example.sh builds it).
cd "$(dirname "$0")/.."
# 2>/dev/null hides nix's "building ...drv" eval chatter so it never lands on
# screen (vty draws straight to the tty, so this doesn't affect the app).
exec nix-shell --run "cabal run -v0 examples -- ${1:-spinner}" 2>/dev/null
