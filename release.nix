{ reflex-platform ? import ./reflex-platform
}:
let
  pkgs = (reflex-platform {}).nixpkgs;
  supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;
  commonOverrides = self: super: {
    vty = self.callHackageDirect {
      pkg = "vty";
      ver = "5.38";
      sha256 = "0kcd3ln9xmc62ka0i7habzvjjar8z63mlvl15rdhf8hqmda0b7r7";
    } {};
  };
in
  lib.genAttrs supportedSystems (system: let
    rp = reflex-platform { inherit system; __useNewerCompiler = true; };
    rpGhc = rp.ghc.override {
      overrides = commonOverrides;
    };
    nixGhc945 = (import ./nixpkgs { inherit system; }).haskell.packages.ghc945.override {
      overrides = self: super: commonOverrides self super // {
        hlint = self.callHackageDirect {
          pkg = "hlint";
          ver = "3.5";
          sha256 = "1np43k54918v54saqqgnd82ccd6225njwxpg2031asi70jam80x9";
        } {};

        # Jailbroken until https://github.com/audreyt/string-qq/pull/3
        string-qq = pkgs.haskell.lib.dontCheck super.string-qq;
        patch = self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.2";
          sha256 = "160zqqhjg48fr3a33gffd82qm3728c8hwf8sn37pbpv82fw71rzg";
        } {};

        reflex = self.callHackageDirect {
          pkg = "reflex";
          ver = "0.9.0.1";
          sha256 = "1yrcashxxclvlvv3cs5gv75rvlsg1gb0m36kssnk2zvhbh94240y";
        } {};
      };
    };
    nixGhc961 = (import ./nixpkgs { inherit system; }).haskell.packages.ghc961.override {
      overrides = self: super: {
        patch = self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.2";
          sha256 = "160zqqhjg48fr3a33gffd82qm3728c8hwf8sn37pbpv82fw71rzg";
        } {};

        reflex = self.callHackageDirect {
          pkg = "reflex";
          ver = "0.9.0.1";
          sha256 = "1yrcashxxclvlvv3cs5gv75rvlsg1gb0m36kssnk2zvhbh94240y";
        } {};
        these-lens = self.callHackageDirect {
          pkg = "these-lens";
          ver = "1.0.1.3";
          sha256 = "0n1vkr57jz5yvy4jm15v5cs42rp342ni0gisib7aqyhibpicqs5c";
        } {};
        these = self.callHackageDirect {
          pkg = "these";
          ver = "1.2";
          sha256 = "1iaaq1fsvg8c3l0czcicshkmbbr00hnwkdamjbkljsa1qvlilaf0";
        } {};
        lens = self.callHackageDirect {
          pkg = "lens";
          ver = "5.2.2";
          sha256 = "0c4a421sxfjm1cj3nvgwkr4glll23mqnsvs2iv5qh85931h2f3cy";
        } {};

        assoc = self.callHackageDirect {
          pkg = "assoc";
          ver = "1.1";
          sha256 = "1krvcafrbj98z5hv55gq4zb1in5yd71nmz9zdiqgnywjzbrvpf75";
        } {};

        strict = self.callHackageDirect {
          pkg = "strict";
          ver = "0.5";
          sha256 = "02iyvrr7nd7fnivz78lzdchy8zw1cghqj1qx2yzbbb9869h1mny7";
        } {};
        vty = self.callHackageDirect {
          pkg = "vty";
          ver = "5.38";
          sha256 = "0kcd3ln9xmc62ka0i7habzvjjar8z63mlvl15rdhf8hqmda0b7r7";
        } {};


        # Jailbroken until https://github.com/audreyt/string-qq/pull/3
        string-qq = haskellLib.dontCheck super.string-qq;
        # Tests aren't compatible with transformers-0.6
        bimap = haskellLib.dontCheck super.bimap;
        exception-transformers = haskellLib.doJailbreak (haskellLib.dontCheck super.exception-transformers);

      };
    };
  in
  {
    ghc810 = rpGhc.callCabal2nix "reflex-vty" (import ./src.nix) {};
    ghc945 = nixGhc945.callCabal2nix "reflex-vty" (import ./src.nix) {};
    ghc961 = nixGhc961.callCabal2nix "reflex-vty" (import ./src.nix) {};
  })
