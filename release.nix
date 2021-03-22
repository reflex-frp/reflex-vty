{ reflex-platform ? import ./reflex-platform {}
}:
let inherit (reflex-platform.nixpkgs.haskell) lib;
    ghc = reflex-platform.ghc.override {
      overrides = self: super: {
        vty = self.callHackage "vty" "5.28" {};
        ansi-terminal = self.callHackage "ansi-terminal" "0.10.3" {};
        ansi-wl-pprint = self.callHackage "ansi-wl-pprint" "0.6.9" {};
        test-framework = lib.doJailbreak super.test-framework;
      };
    };
in
{
  reflex-vty =
    ghc.callCabal2nix "reflex-vty" ./. {};
}
