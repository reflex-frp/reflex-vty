{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, pkgs ? import ./dep/nixpkgs {}
}:
let
  overrides = self: super: {
    reflex = self.callHackageDirect {
      pkg = "reflex";
      ver = "0.9.4.1";
      sha256 = "1igjwcbfcjm7aymb3nwqdwg4sq9zzrp28r77q143ab0bkbjlldcc";
    } {};
    # Local packages. The companion must resolve the reflex-vty of the
    # same package set (self.reflex-vty), not an independently selected
    # Hackage core.
    reflex-vty = self.callCabal2nix "reflex-vty" (import ./src.nix) {};
    reflex-vty-widgets =
      self.callCabal2nix
        "reflex-vty-widgets"
        ((import ./src.nix) + "/reflex-vty-widgets")
        {};
  };
  ghcs = pkgs.lib.genAttrs supportedSystems (system: let
    haskellPackages = (import ./dep/nixpkgs { inherit system; }).haskell.packages;
    ghc98 = haskellPackages.ghc98.override { inherit overrides; };
    ghc910 = haskellPackages.ghc910.override { inherit overrides; };
    ghc912 = haskellPackages.ghc912.override { inherit overrides; };
  in {
    recurseForDerivations = true;
    ghc98 = ghc98.callCabal2nix "reflex-vty" (import ./src.nix) {};
    ghc910 = ghc910.callCabal2nix "reflex-vty" (import ./src.nix) {};
    ghc912 = ghc912.callCabal2nix "reflex-vty" (import ./src.nix) {};
    widgets = {
      recurseForDerivations = true;
      ghc98 = ghc98.reflex-vty-widgets;
      ghc910 = ghc910.reflex-vty-widgets;
      ghc912 = ghc912.reflex-vty-widgets;
    };
  });
in
  ghcs
