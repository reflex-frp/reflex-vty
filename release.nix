{ reflex-platform ? import ./reflex-platform {}
}:
let inherit (reflex-platform.nixpkgs.haskell) lib;
    ghc = reflex-platform.ghc.override {
      overrides = self: super: {
        vty = self.callHackageDirect {
          pkg = "vty";
          ver = "5.37";
          sha256 = "01ppspii5x02iik9rxkf2idi6r9ngax6w3mqi6d8qj765w3cksiq";
        } {};
      };
    };
in
{
  reflex-vty =
    ghc.callCabal2nix "reflex-vty" ./. {};
}
