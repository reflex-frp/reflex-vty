{ reflex-platform ? import ./reflex-platform
}:
let
  pkgs = (reflex-platform {}).nixpkgs;
  supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  inherit (pkgs) lib;
  commonOverrides = self: super: {
    vty = self.callHackageDirect {
      pkg = "vty";
      ver = "5.37";
      sha256 = "01ppspii5x02iik9rxkf2idi6r9ngax6w3mqi6d8qj765w3cksiq";
    } {};
  };
in
  lib.genAttrs supportedSystems (system: let
    rp = reflex-platform { inherit system; __useNewerCompiler = true; };
    rpGhc = rp.ghc.override {
      overrides = commonOverrides;
    };
    nixGhc = (import ./nixpkgs { inherit system; }).haskell.packages.ghc943.override {
      overrides = self: super: commonOverrides self super // {
        hlint = self.callHackageDirect {
          pkg = "hlint";
          ver = "3.5";
          sha256 = "1np43k54918v54saqqgnd82ccd6225njwxpg2031asi70jam80x9";
        } {};
        patch = self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.1";
          sha256 = "0q5rxnyilhbnfph48fnxbclggsbbhs0pkn0kfiadm0hmfr440cgk";
        } {};
        reflex = self.callHackageDirect {
          pkg = "reflex";
          ver = "0.9.0.0";
          sha256 = "0ibashkz3ifwxa61zg3fwamvjwv125l7jhjd74kgjbq13zpx23ib";
        } {};
        # Jailbroken until https://github.com/audreyt/string-qq/pull/3
        string-qq = pkgs.haskell.lib.dontCheck super.string-qq;
      };
    };
  in
  {
    ghc810 = rpGhc.callCabal2nix "reflex-vty" (import ./src.nix) {};
    ghc943 = nixGhc.callCabal2nix "reflex-vty" (import ./src.nix) {};
  })
