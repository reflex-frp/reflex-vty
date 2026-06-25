<div align="center">

# reflex-vty

**Build terminal user interfaces with functional reactive programming.**

reflex-vty provides a [Reflex FRP](https://reflex-frp.org) host and a library of reactive widgets for [Vty](https://hackage.haskell.org/package/vty) terminal applications: layout, text input and editing, boxes, scrolling, mouse support, focus management, and theming.

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-vty.svg)](https://hackage.haskell.org/package/reflex-vty) [![Github CI](https://github.com/reflex-frp/reflex-vty/actions/workflows/haskell.yml/badge.svg)](https://github.com/reflex-frp/reflex-vty/actions) [![Obsidian](https://img.shields.io/badge/Obsidian-Systems-white)](https://obsidian.systems) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)

<img src="https://vhs.charm.sh/vhs-xI6BnH1b2B5zSY9QxXASM.gif" alt="reflex-vty example walkthrough: gradient menu, to-do list, CPU graph, and styling showcase" width="80%">

</div>

### A minimal app

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Vty

main :: IO ()
main = mainWidget def $ do
  text "Hello, reflex-vty! Press Ctrl+C to quit."
  ctrlc
```

`mainWidget` takes a `VtyAppConfig` (`def` for the defaults) and runs a widget until the `Event t ()` it returns fires. Here that is `ctrlc`, which fires when the user presses Ctrl+C. Docs are available on [Hackage](https://hackage.haskell.org/package/reflex-vty/docs/Reflex-Vty.html).

### Features

- **Layout**: arrange widgets into rows and columns (`tile` / `grout`) with fixed or proportional (stretch) size constraints.
- **Focus**: tab-cycling focus management across the focusable widgets in a layout (`tabNavigation`).
- **Text**: word-wrapped text display, rich text with per-span attributes, and single- or multi-line text input backed by a zipper that handles wide characters, tabs, and wrapping.
- **Inputs**: clickable buttons, hyperlinks, and checkboxes.
- **Boxes**: single, thick, double, rounded, ASCII, inner-half, and outer-half border styles, with optional titles and horizontal rules.
- **Scrolling**: scrollable containers with programmatic scrolling, auto-scroll-to-bottom mode, and a visual scrollbar with four visibility modes (always, thumb-only, while-scrolling, hidden).
- **Split panes**: fixed horizontal and vertical splits, plus a mouse-draggable splitter you can resize at runtime (`splitVDrag`).
- **Mouse**: button clicks, drags (with from/to/button/modifier tracking), scroll-wheel events, and last-known mouse-position tracking.
- **Keyboard**: individual key and key-combo events, plus input filtering.
- **Runtime**: alternate-screen mode, terminal cursor control (shape, visibility, and position), bracketed paste, terminal focus tracking, resize, and POSIX signal handling.
- **Theming**: a `Theme` record with presets (default, dark, charm, dracula, nord, zenburn, gruvbox). Widgets inherit the ambient theme and can override locally.
- **Declarative styling**: Lip Gloss-inspired `Style` type with foreground/background colors, text attributes (bold/italic/underline/etc.), padding, margin, borders with per-side colors, width/height constraints, alignment, text transforms, tab expansion, inline mode, whitespace coloring, and border presets.
- **Color**: `RGB` color type, operations (`darken`, `lighten`, `complementary`, `mix`, `alpha`), and gradients (`Gradient1D`, `Gradient2D`).
- **Compositing**: `Reflex.Vty.Canvas` module with per-cell transparency for overlays and layered rendering.
- **Image composition**: `joinHorizontal`/`joinVertical`/`place` utilities for composing rendered images.
- **Color profiles**: automatic terminal color-capability detection (`TrueColor`/`Ansi256`/`Ansi16`/`Ascii`/`NoTTY`) with downsampling.

Run the bundled demo with `cabal run example` to see many of these in action: a text editor, a to-do list, scrollable text, clickable buttons, a live CPU-usage display with a true-color gradient bar, a scrollbar-modes demo, a terminal-cursor demo, and a little styling showcase featuring gradients, color operations, a canvas overlay, border presets, theming, and color-profile downsampling.

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
* Run ghcid for immediate compiler feedback when you save a .hs file: `ghcid -c "cabal repl library:reflex-vty executable:example test:reflex-vty-test --ghc-options=-Wall" -o ghcid-output.txt`
* etc.

##### Selecting a compiler

`nix-shell` defaults to GHC 9.8. The other compilers defined in `release.nix` are `ghc94`, `ghc96`, and `ghc98`. To enter a shell with one of them, pass it as the `compiler` argument:

```bash
nix-shell --argstr compiler ghc96
```

If you were previously building with a different compiler, you may need to run `cabal clean` first.


#### With cabal

Please see the `tested-with` field of the cabal file for known-compatible versions of GHC.

From the reflex-vty project directory:

```bash
# nix-shell -p cabal-install binutils icu # for nix users
cabal build          # to build the library, example, and test suite
cabal repl           # to enter a multi-repl covering all components
cabal repl example   # to enter a repl for the example executable only
```

## About Obsidian Systems

reflex-vty is built and maintained by **[Obsidian Systems](https://obsidian.systems)**. We provide frontier engineering for high-assurance systems: we build production software in Haskell and Nix, and we're long-time stewards of open-source tooling like [Obelisk](https://github.com/obsidiansystems/obelisk), [Reflex](https://reflex-frp.org/), and [nix-thunk](https://github.com/obsidiansystems/nix-thunk).

If you're working with Reflex, terminal or web UIs in Haskell, or Nix and want a partner to help design, build, or ship it, we'd love to hear from you.

- Website: <https://obsidian.systems>
- Blog: <https://blog.obsidian.systems>
- GitHub: <https://github.com/obsidiansystems>

## License

reflex-vty is released under the [BSD-3-Clause License](LICENSE), © 2018 Obsidian Systems LLC.
