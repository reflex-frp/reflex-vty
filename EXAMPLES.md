# reflex-vty examples

Small, standalone demos built on the reflex-vty core — ports of (and riffs on)
the [Bubble Tea examples](https://github.com/charmbracelet/bubbletea/tree/main/examples).
Each is a single short module under [`src-bin/Example/`](src-bin/Example/); run
any of them with:

```bash
cabal run examples -- <name>
```

The widget-flavored demos — `spinner`, `progress`, `stopwatch`, `timer`,
`textinput`, `pager`, `views` — are in the
[README gallery](README.md#example-gallery). This page collects the **canvas
animations**: full-screen effects that lean on per-cell rendering, true-color
gradients, and tick-driven state.

## Canvas animations

### `doom-fire` — the classic Doom fire

A heat grid is seeded from a hot bottom row; every frame each cell cools a little
and drifts upward from the cell below it, and the heat is mapped through a
black→red→orange→yellow→white gradient. ([source](src-bin/Example/DoomFire.hs))

<img src="https://vhs.charm.sh/vhs-2oZX3ZRdUL8CfDTJOLoncL.gif" alt="doom-fire effect" width="560">

### `cellbuffer` — a plasma field

Each cell's character (drawn from a density ramp `" .:-=+*#%@"`) and its color are
a pure function of position and time, summed over a handful of sine waves.
([source](src-bin/Example/CellBuffer.hs))

<img src="https://vhs.charm.sh/vhs-19qq5dSLYbDnWBKu89iTvt.gif" alt="plasma field" width="560">

### `space` — a drifting starfield

Stars with pseudo-random rows and depths warp leftward; faster (nearer) stars are
brighter and drawn with a bolder glyph. ([source](src-bin/Example/Space.hs))

<img src="https://vhs.charm.sh/vhs-4Hdh2yLUIPktuCKZeGoM2m.gif" alt="starfield" width="560">

### `splash` — an animated splash screen

A title types out in a pink→violet→cyan gradient, with a subtitle that fades in
beneath it; the whole thing loops. ([source](src-bin/Example/Splash.hs))

<img src="https://vhs.charm.sh/vhs-1QNXpqEc9fg8E2WKtkDsD9.gif" alt="splash screen" width="560">
