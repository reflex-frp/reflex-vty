# Enter a shell for this project using nixpkgs
{ compiler ? "ghc98"
}:
let
  pkgs = import ./dep/nixpkgs { };
  release = (import ./release.nix {}).${builtins.currentSystem};
in
  pkgs.mkShell {
    name = "shell-${compiler}";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
      pkgs.fourmolu
    ];
    inputsFrom = [
      release.${compiler}.env
    ];
  }
