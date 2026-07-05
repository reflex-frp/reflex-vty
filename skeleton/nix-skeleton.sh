#!/usr/bin/env bash
# nix-skeleton.sh: skeleton a new reflex-vty-based Haskell
# project that builds with nix.
#
# Runs `cabal init -i` in the target directory (producing a
# library & executable skeleton), then layers on:
#   - the nix skeleton (dep/ thunks, release.nix, shell.nix,
#   src.nix, .envrc) parameterized to the project name, and
#   - a minimal reflex-vty executable that draws a greeting exported by the
#   library (the canonical "Hello, reflex-vty!" example from the README).

set -euo pipefail

usage() {
  cat >&2 <<EOF
Usage: $0 -n NAME [DIR]

  -n, --name NAME   Package name (required). Used for the cabal project name
                    and substituted into the generated nix and Haskell files.
  DIR               Target directory (default: current directory).
                    Created if it does not exist.
EOF
  exit 2
}

NAME=""
DIR="."

while [ "$#" -gt 0 ]; do
  case "$1" in
    -n|--name) NAME="${2:-}"; shift 2 ;;
    -n*) NAME="${1#-n}"; shift ;;
    --name=*) NAME="${1#--name=}"; shift ;;
    -h|--help) usage ;;
    --) shift; break ;;
    -*) echo "unknown option: $1" >&2; usage ;;
    *) DIR="$1"; shift ;;
  esac
done

[ -n "$NAME" ] || { echo "error: -n/--name is required" >&2; usage; }

# Validate the name looks like a cabal package name.
case "$NAME" in
  *[!A-Za-z0-9-]*|[!A-Za-z0-9]*) echo "error: invalid package name '$NAME'" >&2; exit 2 ;;
esac

# Resolve the template directory (this script's sibling ./template).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEMPLATE_DIR="$SCRIPT_DIR/template"

if [ ! -d "$TEMPLATE_DIR" ]; then
  echo "error: template directory not found at $TEMPLATE_DIR" >&2
  exit 1
fi

mkdir -p "$DIR"
DIR="$(cd "$DIR" && pwd)"

# 1. cabal init -i  (interactive; produces library + executable skeleton).
if [ -n "$(find "$DIR" -maxdepth 1 -name '*.cabal' -print -quit)" ]; then
  echo "==> existing .cabal file found; skipping cabal init" >&2
else
  echo "==> running 'cabal init -i' in $DIR" >&2
  (cd "$DIR" && cabal init -i --package-name "$NAME" --libandexe)
fi

CABAL_FILE="$DIR/$NAME.cabal"
if [ ! -f "$CABAL_FILE" ]; then
  echo "error: expected $CABAL_FILE to exist after cabal init" >&2
  exit 1
fi

# 2. Replace the library + executable sources with the reflex-vty example.
#    The library exports a trivial `greeting`; the executable runs mainWidget.
echo "==> installing reflex-vty example sources" >&2
mkdir -p "$DIR/src" "$DIR/app"
rm -f "$DIR"/src/*.hs
sed "s/@PACKAGE_NAME@/$NAME/g" "$TEMPLATE_DIR/src/App.hs" > "$DIR/src/App.hs"
sed "s/@PACKAGE_NAME@/$NAME/g" "$TEMPLATE_DIR/app/Main.hs" > "$DIR/app/Main.hs"

# 3. Patch the cabal file: expose `App`, and add reflex-vty to the executable.
echo "==> patching $NAME.cabal" >&2
sed -i "s/^[[:space:]]*exposed-modules:.*/    exposed-modules:      App/" "$CABAL_FILE"
# The reflex-vty host needs the threaded RTS (it spawns input/vty threads).
sed -i "s/ghc-options: -Wall/ghc-options: -Wall -threaded -rtsopts/" "$CABAL_FILE"

# Append deps after the self/base dependency lines:
#   - in the executable: `, reflex-vty` (after the package's self-dependency)
#   - in the library: append `, text` to the build-depends line (App.hs needs Data.Text)
awk -v pkg="$NAME" '
  /^library([[:space:]]|$)/                                   { in_lib = 1; in_exe = 0 }
  /^executable[[:space:]]/                                    { in_lib = 0; in_exe = 1 }
  /^(test-suite|common|benchmark)([[:space:]]|$)/             { in_lib = 0; in_exe = 0 }
  in_lib && /^[[:space:]]*build-depends:[[:space:]].*base/ {
    sub(/[[:space:]]+$/, "")
    $0 = $0 ", text"
  }
  { print }
  in_exe {
    line = $0
    sub(/^[[:space:]]+/, "", line)
    sub(/[[:space:]]+$/, "", line)
    if (line == pkg) {
      print "        , reflex-vty"
      print "        , vty"
      print "        , text"
    }
  }
' "$CABAL_FILE" > "$CABAL_FILE.tmp" && mv "$CABAL_FILE.tmp" "$CABAL_FILE"

# 4. Lay down the nix scaffolding, substituting @PACKAGE_NAME@ -> NAME.
echo "==> installing nix scaffolding into $DIR" >&2
while IFS= read -r -d '' rel; do
  rel="${rel#$TEMPLATE_DIR/}"
  # Skip the Haskell sources we already handled above.
  case "$rel" in
    src/*|app/*) continue ;;
  esac
  src="$TEMPLATE_DIR/$rel"
  dst="$DIR/$rel"
  mkdir -p "$(dirname "$dst")"
  # Plain text files get @PACKAGE_NAME@ substituted; thunk payloads contain no
  # such token, so sed is a harmless no-op on them.
  sed "s/@PACKAGE_NAME@/$NAME/g" "$src" > "$dst"
  if [ -x "$src" ]; then chmod +x "$dst"; fi
done < <(find "$TEMPLATE_DIR" -type f -print0)

echo "==> done"
echo "    cd $DIR && nix-shell --run 'cabal run'"
