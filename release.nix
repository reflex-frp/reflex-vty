{ reflex-platform-fun ? import ./reflex-platform
}: {
  reflex-vty =
    (reflex-platform-fun {}).ghc.callCabal2nix "reflex-vty" ./. {};
}
