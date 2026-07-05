# Enter a nix-shell for this project.
{ compiler ? "ghc98"
, profiling ? false # build closure with library profiling (for -hc/-hy/-p)
}:
let
  pkgs = import ./dep/nixpkgs { };
  release = (import ./release.nix {}).${builtins.currentSystem};
  hp =
    if profiling
      then release.${compiler + "Packages"}.extend (self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      })
      else release.${compiler + "Packages"};
  pkg = hp.callCabal2nix "@PACKAGE_NAME@" (import ./src.nix) { };
in
  pkgs.mkShell {
    name = "shell-${compiler}" + pkgs.lib.optionalString profiling "-profiling";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
      pkgs.fourmolu
    ];
    inputsFrom = [
      pkg.env
    ];
  }
