# reflex-vty examples

Tech demos built on the reflex-vty core. Some of these are ports of (and riffs on)
the [Bubble Tea examples](https://github.com/charmbracelet/bubbletea/tree/main/examples).
Each is a module under [`src-bin/Example/`](src-bin/Example/) and can be run with:

```bash
cabal run examples -- <name>
```

## Canvas animations

### `doom-fire`: the classic Doom fire

A heat grid seeded from a "hot" bottom row. Every frame each cell cools a little
and drifts upward from the cell below it, and the heat is mapped through a
black-red-orange-yellow-white gradient. ([source](src-bin/Example/DoomFire.hs))

<img src="https://vhs.charm.sh/vhs-2oZX3ZRdUL8CfDTJOLoncL.gif" alt="doom-fire effect" width="560">

### `cellbuffer`: a plasma field

Each cell's character (ordered by density: `" .:-=+*#%@"`) and its color are
a function of position and time, summed over a handful of sine waves.
([source](src-bin/Example/CellBuffer.hs))

<img src="https://vhs.charm.sh/vhs-19qq5dSLYbDnWBKu89iTvt.gif" alt="plasma field" width="560">

### `space`: a drifting starfield

Stars with pseudorandom rows and depths warp leftward. Faster (nearer) stars are
brighter and drawn with a bolder char. ([source](src-bin/Example/Space.hs))

<img src="https://vhs.charm.sh/vhs-4Hdh2yLUIPktuCKZeGoM2m.gif" alt="starfield" width="560">

### `splash`: an animated splash screen

Introducing... ([source](src-bin/Example/Splash.hs))

<img src="https://vhs.charm.sh/vhs-1QNXpqEc9fg8E2WKtkDsD9.gif" alt="splash screen" width="560">
