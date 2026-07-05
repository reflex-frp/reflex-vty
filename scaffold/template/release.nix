{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, pkgs ? import ./dep/nixpkgs {}
}:
let
  inherit (pkgs) lib;
  ghcs = lib.genAttrs supportedSystems (system: let
    hp = (import ./dep/nixpkgs { inherit system; }).haskell.packages.ghc98;
  in {
    recurseForDerivations = true;
    ghc98 = hp.callCabal2nix "@PACKAGE_NAME@" (import ./src.nix) {};
    ghc98Packages = hp;
  });
in
  ghcs
