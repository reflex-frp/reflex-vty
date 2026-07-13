{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, pkgs ? import ./dep/nixpkgs {}
}:
let
  inherit (pkgs) lib;
  overrides = self: super: {
    reflex = self.callHackageDirect {
      pkg = "reflex";
      ver = "0.9.4.1";
      sha256 = "1igjwcbfcjm7aymb3nwqdwg4sq9zzrp28r77q143ab0bkbjlldcc";
    } {};
    reflex-vty = self.callCabal2nix "reflex-vty" (builtins.fetchGit {
      url = "https://github.com/reflex-frp/reflex-vty.git";
      rev = "@REFLEX_VTY_REV@";
    }) {};
  };
  ghcs = lib.genAttrs supportedSystems (system: let
    haskellPackages = (import ./dep/nixpkgs { inherit system; }).haskell.packages;
    ghc98 = haskellPackages.ghc98.override { inherit overrides; };
    ghc910 = haskellPackages.ghc910.override { inherit overrides; };
    ghc912 = haskellPackages.ghc912.override { inherit overrides; };
  in {
    recurseForDerivations = true;
    ghc98 = ghc98.callCabal2nix "@PACKAGE_NAME@" (import ./src.nix) {};
    ghc98Packages = ghc98;
    ghc910 = ghc910.callCabal2nix "@PACKAGE_NAME@" (import ./src.nix) {};
    ghc910Packages = ghc910;
    ghc912 = ghc912.callCabal2nix "@PACKAGE_NAME@" (import ./src.nix) {};
    ghc912Packages = ghc912;
  });
in
  ghcs
