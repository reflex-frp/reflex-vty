{ p ? import ./reflex-platform/stable {}
}:
let
  inherit (p.nixpkgs) lib;
  platforms = p.thunkSet ./reflex-platform;
in (lib.mapAttrs (name: platform: (import platform {}).ghc.callCabal2nix "reflex-vty" ./. {})) platforms 
