# reflex-vty

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-vty.svg)](https://hackage.haskell.org/package/reflex-vty) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/reflex-vty/blob/master/LICENSE)

Build terminal applications using functional reactive programming (FRP) with [Reflex FRP](https://reflex-frp.org).

![Example Animation](https://i.imgur.com/FULQNtu.gif)

Feature requests, pull requests, and other feedback are welcome and appreciated (see the [contribution guide](CONTRIBUTING.md)). This library
is still experimental, so big changes are possible!
### How to Build

#### With reflex-platform

Enter a nix-shell for the project:
```bash
git clone https://github.com/reflex-frp/reflex-vty.git
cd reflex-vty
nix-shell
```

From within the nix-shell you can:
* Run the example: `cabal repl example`
* Load the library in the repl: `cabal repl reflex-vty`
* Build the example executable: `cabal build example`
* Build the docs: `cabal haddock`
* Run ghcid for immediate compiler feedback when you save a .hs file: `ghcid -c "cabal repl reflex-vty --ghc-options=-Wall"`
* etc.

##### Selecting a compiler

When entering the nix-shell, you can select from the following compilers: ghc-8.10.7 and ghc-9.4.3. By default, ghc-8.10.7 is selected. To enter a shell with ghc-9.4.3, run:

```bash
nix-shell --argstr compiler ghc943
```

You may need to run `cabal clean` and `cabal configure -w ghc-9.4.3` if you were previously working on the project with a different compiler.


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
