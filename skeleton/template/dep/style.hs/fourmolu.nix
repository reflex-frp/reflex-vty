{ inputs ? (import ./inputs.nix)
, system ? builtins.currentSystem
, pkgs ? (import inputs.nixpkgs { inherit system; })
}:

pkgs.writeShellScriptBin "fourmolu" ''
  baseConfig=${./fourmolu.yaml}

  # Forward all arguments to fourmolu, but collapse repeated `--config` flags to
  # the last one (fourmolu itself rejects duplicates). With no `--config`, fall
  # back to the bundled config; `--config -` reads the config from stdin.
  args=()
  config=$baseConfig
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --config)
        config=$2
        shift 2
        ;;
      --config=*)
        config=''${1#--config=}
        shift
        ;;
      *)
        args+=("$1")
        shift
        ;;
    esac
  done

  if [ "$config" = "-" ]; then
    config=$(${pkgs.coreutils}/bin/mktemp)
    trap '${pkgs.coreutils}/bin/rm -f "$config"' EXIT
    ${pkgs.coreutils}/bin/cat - > "$config"
    ${pkgs.lib.getExe pkgs.fourmolu} --config "$config" "''${args[@]}"
  else
    exec ${pkgs.lib.getExe pkgs.fourmolu} --config "$config" "''${args[@]}"
  fi
''
