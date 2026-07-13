#!/usr/bin/env bash

set -euo pipefail

: "${AWS_ACCESS_KEY_ID:?AWS_ACCESS_KEY_ID must be set}"
: "${AWS_SECRET_ACCESS_KEY:?AWS_SECRET_ACCESS_KEY must be set}"
: "${NIX_CACHE_PRIVATE_KEY:?NIX_CACHE_PRIVATE_KEY must be set}"

if [[ -f "$AWS_ACCESS_KEY_ID" ]]; then
  echo "AWS_ACCESS_KEY_ID contains a file path; export the file's contents instead" >&2
  exit 1
fi
if [[ "$AWS_ACCESS_KEY_ID" == */* || "$AWS_ACCESS_KEY_ID" == *$'\n'* ]]; then
  echo "AWS_ACCESS_KEY_ID is not a valid key ID" >&2
  exit 1
fi
if [[ -f "$AWS_SECRET_ACCESS_KEY" ]]; then
  echo "AWS_SECRET_ACCESS_KEY contains a file path; export the file's contents instead" >&2
  exit 1
fi

cache_url="${NIX_CACHE_URL:-s3://nixcache.reflex-frp.org}"
iog_cache="https://cache.iog.io"
iog_public_key="hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
reflex_cache="https://nixcache.reflex-frp.org"
repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
key_file="$(mktemp)"
outputs_file="$(mktemp)"
derivations_file="$(mktemp)"
paths_file="$(mktemp)"

cleanup() {
  rm -f "$key_file" "$outputs_file" "$derivations_file" "$paths_file"
}
trap cleanup EXIT

printf '%s\n' "$NIX_CACHE_PRIVATE_KEY" > "$key_file"
chmod 600 "$key_file"

nix-build "$repo_root/release.nix" "$@" --no-out-link \
  --option extra-substituters "$iog_cache $reflex_cache" \
  --option extra-trusted-public-keys "$iog_public_key" \
  > "$outputs_file"
mapfile -t outputs < "$outputs_file"

if (( ${#outputs[@]} == 0 )); then
  echo "release.nix produced no outputs" >&2
  exit 1
fi

nix-store --query --deriver "${outputs[@]}" > "$derivations_file"
mapfile -t derivations < "$derivations_file"

if (( ${#derivations[@]} == 0 )); then
  echo "release.nix produced no derivations" >&2
  exit 1
fi

nix-store --query --requisites --include-outputs "${derivations[@]}" \
  | sort -u > "$paths_file"
mapfile -t paths < "$paths_file"

if (( ${#paths[@]} == 0 )); then
  echo "release.nix produced an empty derivation closure" >&2
  exit 1
fi

nix store sign --recursive --key-file "$key_file" "${paths[@]}"
nix copy --to "$cache_url" "${paths[@]}"
