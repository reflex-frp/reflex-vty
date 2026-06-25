#!/usr/bin/env bash
# Record the README walkthrough GIF for reflex-vty using VHS.
#
# - Pre-builds the `example` with the project's pinned toolchain so the
#   in-recording `cabal run` is instant.
# - Runs VHS from the ambient nixpkgs channel (the pinned dep/nixpkgs may not
#   contain vhs); ttyd, chromium and ffmpeg are already available.
#
# Output: doc/reflex-vty.gif
set -euo pipefail
cd "$(dirname "$0")/.."

echo "==> Building example (project nix-shell)…"
nix-shell --run 'cabal build example'

echo "==> Recording with VHS…"
nix-shell -p vhs --run 'vhs doc/demo.tape'

echo "==> Wrote doc/reflex-vty.gif"
