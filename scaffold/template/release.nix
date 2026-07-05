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
    reflex-vty = self.callHackageDirect {
      pkg = "reflex-vty";
      ver = "1.1.0.0";
      sha256 = "11jj70q9mjv20m03pgpismns462dkzp50pmqg4f6h3wpzwh330wf";
    } {};
  };
  ghcs = lib.genAttrs supportedSystems (system: let
    hp = (import ./dep/nixpkgs { inherit system; }).haskell.packages.ghc98.override { inherit overrides; };
  in {
    recurseForDerivations = true;
    ghc98 = hp.callCabal2nix "@PACKAGE_NAME@" (import ./src.nix) {};
    ghc98Packages = hp;
  });
in
  ghcs
