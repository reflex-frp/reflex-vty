# reflex-vty

## A library for building functional reactive terminal applications

![reflex-vty example animation](doc/welcome.gif)

### How to Build

#### With reflex-platform

Enter a nix-shell for the project:
```bash
git clone git@github.com:reflex-frp/reflex-platform
git clone git@gitlab.com:obsidian.systems/reflex-vty
cd reflex-vty
../reflex-platform/scripts/work-on ghc ./.
```

From within the nix-shell you can:
* Run the example: `cabal repl example`
* Load the library in the repl: `cabal repl reflex-vty`
* Build the example executable: `cabal build example`
* Build the docs: `cabal haddock`
* etc.

### Contributing

Feature requests, pull requests, and other feedback are welcome and appreciated. This library
is still experimental, so big changes are possible!

Please make sure contributions have 100% haddocks coverage.
