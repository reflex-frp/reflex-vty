# Enter a shell for this project using reflex-platform (which provides ghc8107)
# or nixpkgs (which provides ghc943)
{ compiler ? "ghc98" # or "ghc943"
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
