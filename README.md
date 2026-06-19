<div align="center">

# reflex-vty

**Build terminal user interfaces with functional reactive programming.**

reflex-vty provides a [Reflex FRP](https://reflex-frp.org) host and a library of reactive widgets for [Vty](https://hackage.haskell.org/package/vty) terminal applications: layout, text input and editing, boxes, scrolling, mouse support, focus management, and theming.

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-vty.svg)](https://hackage.haskell.org/package/reflex-vty) [![Github CI](https://github.com/reflex-frp/reflex-vty/actions/workflows/haskell.yml/badge.svg)](https://github.com/reflex-frp/reflex-vty/actions) [![Obsidian](https://img.shields.io/badge/Obsidian-Systems-white)](https://obsidian.systems) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)

<img src="https://i.imgur.com/FULQNtu.gif" alt="reflex-vty example animation" width="80%">

</div>

Feature requests, pull requests, and other feedback are welcome and appreciated (see the [contribution guide](CONTRIBUTING.md)). This library is still experimental, so big changes are possible.

### How to Build

#### With reflex-platform

Enter a nix-shell for the project:
```bash
git clone https://github.com/reflex-frp/reflex-vty.git
cd reflex-vty
nix-shell
```

From within the nix-shell you can:
* Run the example: `cabal run example`
* Load the library in the repl: `cabal repl reflex-vty`
* Build the example executable: `cabal build example`
* Build the docs: `cabal haddock`
* Run ghcid for immediate compiler feedback when you save a .hs file: `ghcid -c "cabal repl reflex-vty --ghc-options=-Wall"`
* etc.

##### Selecting a compiler

`nix-shell` defaults to GHC 9.8. The other compilers defined in `release.nix` are `ghc810`, `ghc94`, and `ghc96`. To enter a shell with one of them, pass it as the `compiler` argument:

```bash
nix-shell --argstr compiler ghc810
```

If you were previously building with a different compiler, you may need to run `cabal clean` first.


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

## About Obsidian Systems

reflex-vty is built and maintained by **[Obsidian Systems](https://obsidian.systems)**. We provide frontier engineering for high-assurance systems: we build production software in Haskell and Nix, and we're long-time stewards of open-source tooling like [Obelisk](https://github.com/obsidiansystems/obelisk), [Reflex](https://reflex-frp.org/), and [nix-thunk](https://github.com/obsidiansystems/nix-thunk).

If you're working with Reflex, terminal or web UIs in Haskell, or Nix and want a partner to help design, build, or ship it, we'd love to hear from you.

- Website: <https://obsidian.systems>
- Blog: <https://blog.obsidian.systems>
- GitHub: <https://github.com/obsidiansystems>

## License

reflex-vty is released under the [BSD-3-Clause License](LICENSE), © 2018 Obsidian Systems LLC.
