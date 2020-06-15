# reflex-vty

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-vty.svg)](https://hackage.haskell.org/package/reflex-vty) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/reflex-vty/badge)](https://matrix.hackage.haskell.org/#/package/reflex-vty) [![Travis CI](https://api.travis-ci.org/reflex-frp/reflex-vty.svg?branch=develop)](https://travis-ci.org/reflex-frp/reflex-vty) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/reflex-vty/blob/master/LICENSE)

Build terminal applications using functional reactive programming (FRP) with [Reflex FRP](https://reflex-frp.org).

![Example Animation](https://i.imgur.com/FULQNtu.gif)

Feature requests, pull requests, and other feedback are welcome and appreciated (see the [contribution guide](CONTRIBUTING.md)). This library
is still experimental, so big changes are possible!
### How to Build

#### With reflex-platform

Enter a nix-shell for the project:
```bash
git clone git@github.com:reflex-frp/reflex-platform
git clone git@github.com:reflex-frp/reflex-vty
cd reflex-vty
../reflex-platform/scripts/work-on ghc ./.
```

From within the nix-shell you can:
* Run the example: `cabal repl example`
* Load the library in the repl: `cabal repl reflex-vty`
* Build the example executable: `cabal build example`
* Build the docs: `cabal haddock`
* Run ghcid for immediate compiler feedback when you save a .hs file: `ghcid -c "cabal repl reflex-vty --ghc-options=-Wall"`
* etc.

#### With cabal

Please see the `tested-with` field of the cabal file for known-compatible versions of GHC.

From the reflex-vty project directory:

```bash
# nix-shell -p cabal-install binutils icu # for nix users
cabal new-configure
cabal new-build # to build the library and example
cabal new-repl # to enter a repl for the library
cabal new-repl example # to enter a repl for the example executable
```
