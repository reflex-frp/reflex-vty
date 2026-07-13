{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, pkgs ? import ./dep/nixpkgs {}
}:
let
  ghcs = pkgs.lib.genAttrs supportedSystems (system: let
    haskellPackages = (import ./dep/nixpkgs { inherit system; }).haskell.packages;
  in {
    recurseForDerivations = true;
    ghc98 = haskellPackages.ghc98.callCabal2nix "reflex-vty" (import ./src.nix) {};
    ghc910 = haskellPackages.ghc910.callCabal2nix "reflex-vty" (import ./src.nix) {};
    ghc912 = haskellPackages.ghc912.callCabal2nix "reflex-vty" (import ./src.nix) {};
  });
in
  ghcs
