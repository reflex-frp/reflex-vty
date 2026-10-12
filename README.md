<div align="center">

# reflex-vty

**Build terminal user interfaces with functional reactive programming.**

reflex-vty provides a [Reflex FRP](https://reflex-frp.org) host and a library of reactive widgets for [Vty](https://hackage.haskell.org/package/vty) terminal applications: layout, text input and editing, boxes, scrolling, mouse support, focus management, and theming.

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/reflex-vty.svg)](https://hackage.haskell.org/package/reflex-vty) [![Github CI](https://github.com/reflex-frp/reflex-vty/actions/workflows/haskell.yml/badge.svg)](https://github.com/reflex-frp/reflex-vty/actions) [![Obsidian](https://img.shields.io/badge/Obsidian-Systems-white)](https://obsidian.systems) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)

<img src="https://vhs.charm.sh/vhs-4ch4hhuTGZMXLIOPgNcA0G.gif" alt="reflex-vty example walkthrough: gradient menu, to-do list, scrollbar modes, and styling showcase" width="80%">

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

### Example gallery

Each of these examples is a module in [`src-bin/Example/`](src-bin/Example/). You can run any of them with `cabal run examples -- <name>`.

|                                                 |                                                                                                     |
| :--                                             | :--                                                                                                 |
| **`spinner`** — an animated braille spinner     | <img src="https://vhs.charm.sh/vhs-2ZldM9gFD9ovea54MAwio3.gif" alt="spinner demo" width="420">      |
| **`progress`** — a gradient-filled progress bar | <img src="https://vhs.charm.sh/vhs-3oJNQQcQH6vM4VyH7Yww56.gif" alt="progress bar demo" width="420"> |
| **`stopwatch`** — a tick-driven count-up timer  | <img src="https://vhs.charm.sh/vhs-5C8hkG3W9P4tATBBtLhxIm.gif" alt="stopwatch demo" width="420">    |
| **`timer`** — a count-down timer                | <img src="https://vhs.charm.sh/vhs-3gpq3fy1zWP3IsXysnT7WT.gif" alt="timer demo" width="420">        |
| **`textinput`** — a focused text-input field    | <img src="https://vhs.charm.sh/vhs-4FzSVaJAnGq4V1NNrk1lal.gif" alt="text input demo" width="420">   |
| **`pager`** — a scrollable text viewport        | <img src="https://vhs.charm.sh/vhs-7a2bCmFHleF9MG3KxY4yeh.gif" alt="pager demo" width="420">        |
| **`views`** — toggle between two views          | <img src="https://vhs.charm.sh/vhs-7rflGBcgFrmIQgk0JmZNKe.gif" alt="views demo" width="420">        |

**More demos**: full-screen canvas animations (the classic Doom fire, a plasma field, a starfield, an animated splash) live on the [examples page](EXAMPLES.md).

Feature requests, pull requests, and other feedback are welcome and appreciated (see the [contribution guide](CONTRIBUTING.md)).

### How to Build

#### With reflex-platform

Prebuilt Nix dependencies are available from `https://nixcache.reflex-frp.org`. NixOS users can enable the cache by following the [binary cache setup instructions](https://github.com/reflex-frp/reflex-platform/blob/f70e481fa59ecb7958abeb2cb928c79efcd65f43/notes/NixOS.md).

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

`nix-shell` defaults to GHC 9.12. The compilers defined in `release.nix` are `ghc98`, `ghc910`, and `ghc912`. To enter a shell with one of them, pass it as the `compiler` argument:

```bash
nix-shell --argstr compiler ghc98
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

### Tests and terminal checks

```bash
REFLEX_VTY_GOLDEN_UPDATE=0 cabal test all --test-show-details=direct
cabal build -ftest-tools reflex-vty-test-driver
python3 test/pty_smoke.py "$(cabal list-bin -ftest-tools reflex-vty-test-driver)"
```

The portable suite uses an in-memory Vty to run the production host.
It compares rendered spans, including wide characters, combining marks, and attributes.
Missing or different golden files cause test failures. Tests do not change these files in normal mode.
To replace local golden files, use this command:

```bash
REFLEX_VTY_GOLDEN_UPDATE=1 cabal test reflex-vty-test --test-option=--match=Golden
```

Review each changed fixture before you commit it. Update mode cannot run when `CI=true`.
The source-distribution CI job checks fixture bytes in the package.
It then runs the suite from the extracted package.

The PTY checks require POSIX, Python 3, a UTF-8 locale, and the `xterm-256color` terminfo entry.
They check terminal input, paste, resize, and scrolling.
They also check terminal restoration after normal exits, signals, and exceptions.
Windows runs the portable suite and separate console-handler checks.
These tests do not check interactive ConPTY operation.

Coverage includes every declared module in both libraries, including modules that did not execute.
`test/Coverage.hs` compares expression, boolean-pair, and alternative ratios with `test/coverage-baseline.tsv` for each module.
These ratios do not prove complete behavioral coverage.
Use GHC **9.12.3** with optimization disabled and a new instrumented build directory for baseline comparisons.
Use the coverage commands in `.github/workflows/haskell.yml`.
The reporting tool must use the Cabal version that wrote `setup-config`.
The workflow specifies Cabal **3.16.1.0**. GHC can supply a different Cabal version.
Install the specified Cabal library in a separate package environment:

```bash
cabal install --lib Cabal-3.16.1.0 extra --package-env=/tmp/reflex-vty-coverage.env
```

Before you run the instrumented suite, create an empty `dist-coverage/child-tix` directory.
Create `dist-coverage/child-tix/run-start` with `touch`.
Set `REFLEX_VTY_COVERAGE_CHILD_DIR` to the absolute path of the child directory and export the variable.
Keep this variable set during analysis.
The tool rejects unknown child files.
It also rejects child tick files older than the marker or newer than the parent tick file.
After the suite passes, use this command to update the local baseline:

```bash
runghc -package-env=/tmp/reflex-vty-coverage.env \
  -package=Cabal -package=hpc -package=extra -package=directory \
  -package=filepath -package=process -package=time -package=containers \
  test/Coverage.hs update dist-coverage test/coverage-baseline.tsv dist-coverage/report
```

Review the baseline changes before you commit them.
Use `check` instead of `update` to compare coverage with the committed baseline.
Analysis does not change the tick files. Repeated checks use the same data.
Baseline updates cannot run when `CI=true`.

## About Obsidian Systems

reflex-vty is built and maintained by **[Obsidian Systems](https://obsidian.systems)**. We provide frontier engineering for high-assurance systems: we build production software in Haskell and Nix, and we're long-time stewards of open-source tooling like [Obelisk](https://github.com/obsidiansystems/obelisk), [Reflex](https://reflex-frp.org/), and [nix-thunk](https://github.com/obsidiansystems/nix-thunk).

If you're working with Reflex, terminal or web UIs in Haskell, or Nix and want a partner to help design, build, or ship it, we'd love to hear from you.

- Website: <https://obsidian.systems>
- Blog: <https://blog.obsidian.systems>
- GitHub: <https://github.com/obsidiansystems>

## License

reflex-vty is released under the [BSD-3-Clause License](LICENSE), © 2018 Obsidian Systems LLC.
