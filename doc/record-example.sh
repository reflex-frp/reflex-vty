#!/usr/bin/env bash
# Record a gallery demo GIF with VHS.  Usage: record-example.sh <name>
# (<name> must have a matching doc/examples/<name>.tape). Output:
# doc/examples/<name>.gif — publish it with `vhs publish` and use the URL.
set -euo pipefail
cd "$(dirname "$0")/.."
name="${1:?usage: record-example.sh <name>}"
echo "==> Building examples…"
nix-shell --run 'cabal build examples'
echo "==> Recording doc/examples/${name}.gif…"
nix-shell -p vhs --run "vhs doc/examples/${name}.tape"
echo "==> Done: doc/examples/${name}.gif"
