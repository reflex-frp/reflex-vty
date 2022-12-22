# Enter a shell for this project using reflex-platform (which provides ghc8107)
# or nixpkgs (which provides ghc943)
{ compiler ? "ghc810" # or "ghc943"
}:
let pkgs = (import ./reflex-platform { }).nixpkgs;
in
  pkgs.mkShell {
    name = "shell-${compiler}";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      (import ./release.nix {}).${builtins.currentSystem}.${compiler}.env
    ];
  }
