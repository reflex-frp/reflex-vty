# QA Guide

Use this guide for automated tests and manual terminal checks.

Run these commands from the repository root.

## Test suite

```bash
REFLEX_VTY_GOLDEN_UPDATE=0 cabal test all --test-show-details=direct
```

The portable suite uses an in-memory Vty to run the production host.
It compares rendered spans, including wide characters, combining marks, and attributes.

## Golden files

Missing or different golden files cause test failures. Tests do not change these files in normal mode.
To replace local golden files, use this command:

```bash
REFLEX_VTY_GOLDEN_UPDATE=1 cabal test reflex-vty-test --test-option=--match=Golden
```

Review each changed fixture before you commit it. Update mode cannot run when `CI=true`.
The source-distribution CI job checks fixture bytes in the package.
It then runs the suite from the extracted package.

## Terminal checks

```bash
cabal build -ftest-tools reflex-vty-test-driver
python3 test/pty_smoke.py "$(cabal list-bin -ftest-tools reflex-vty-test-driver)"
```

The PTY checks require POSIX, Python 3, a UTF-8 locale, and the `xterm-256color` terminfo entry.
They check terminal input, paste, resize, and scrolling.
They also check terminal restoration after normal exits, signals, and exceptions.
Windows runs the portable suite and separate console-handler checks.
These tests do not check interactive ConPTY operation.

## Coverage

Coverage includes every declared module in both libraries, including modules that did not execute.
`test/Coverage.hs` compares expression, boolean-pair, and alternative ratios with `test/coverage-baseline.tsv` for each module.
These ratios do not prove complete behavioral coverage.
Use GHC **9.12.3** with optimization disabled and a new instrumented build directory for baseline comparisons.
Use the coverage commands in [the CI workflow](../.github/workflows/haskell.yml).

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

## Manual checks

These checks require Nix.

Clone and examples

```bash
nix-shell -p git
cd $(mktemp -d)
git clone https://github.com/reflex-frp/reflex-vty.git --branch develop
nix run -f reflex-vty/release.nix reflex-vty --command example
```

The build may take a while to complete.

Once it's done you can test the following:

  * On the main menu, make sure the top line shows a "reflex-vty · functional reactive terminal UIs" banner whose characters fade through a pink → violet → cyan true-color gradient.

  * Make sure that resizing your terminal window causes the editor to resize as well.

  * Click on `Todo List`.
    * Make sure a cyan "To-Do · Tab to move · Space/click/Ctrl+T toggles · Enter adds a task" header appears above the list.
    * Make sure you can check and uncheck a task by clicking its checkbox, or by focusing the checkbox (see the focus indicators below) and pressing <kbd>Space</kbd>. Make sure you can add new tasks and edit the text of tasks.
    * Make sure pressing <kbd>Enter</kbd> while a row is focused inserts a new empty task directly beneath that row (not at the bottom of the list) and moves focus to it, so you can type immediately. The other rows keep their text and checkbox state. Pressing <kbd>Enter</kbd> on the last row, or clicking "Add another task", appends at the end.
    * Make sure pressing <kbd>Ctrl</kbd>+<kbd>T</kbd> toggles the focused row's checkbox from anywhere in the row, including while the text field is focused (and that it does not type a literal character into the text field).
    * Make sure a cyan "▸" caret appears in the left gutter of the focused row, and that pressing <kbd>Tab</kbd> / <kbd>Shift</kbd>+<kbd>Tab</kbd> moves it from row to row (and between the checkbox and the text field within a row).
    * Make sure that, within the focused row, you can tell which element has focus: the checkbox is drawn in reverse video when it is focused, and a reverse-video cursor block appears in the text field when the text field is focused — including at the very end of the text and in an empty (newly added) task, where it sits on a blank cell.
    * Make sure that resizing your terminal window causes the editor to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Text Editor`.
    * Make sure you can edit text in the "Text Edit" box. Try entering lots of text so that it doesn't all fit. It should scroll down as you type. Using they arrow keys on your keyboard should move the cursor and let you go back up and see text that was out of view before.
    * Make sure that clicking, right-clicking, using the scroll-wheel on your mouse, and dragging in the "Text Edit" box updates the message in the box lying above and behind the "Text Edit" box.
    * Make sure that dragging your mouse cursor around inside the lowest box causes the message to update. This should update only when dragging *inside* that box, not outside of it.
    * Make sure that resizing your terminal window causes the boxes to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Scrollable text display`.
    * Make sure that you can scroll up and down through the small box of text with your mouse's scroll wheel. The box has a rounded, cyan-tinted border titled "De Bello Gallico", and the Latin body text stays in the default (un-tinted) color.
    * Make sure that the second box, "This one scrolls automatically as the output grows", appends one timestamped line per second and stays scrolled to the bottom as new lines appear.
    * Make sure that the "Height", "Scroll", and "Length" readouts below (inside a cyan-tinted box titled "Scroll state") update as you scroll and as new lines arrive.
    * Make sure that resizing your terminal window causes the boxes to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Clickable buttons`.
    * Make sure that clicking each of the nine buttons (or focusing one and pressing <kbd>Enter</kbd> or <kbd>Space</kbd>) appends its associated emoji into the "CLICK BUTTONS TO DRAW" box.
    * Make sure the "CLICK BUTTONS TO DRAW" box has a rounded, cyan-tinted border and title.
    * Make sure the focused button is rendered with a double-line border; unfocused buttons use the default single-line border.
    * Type <kbd>Esc</kbd>.

  * Click on `CPU Usage`.
    * Make sure a vertical bar appears inside the titled box and updates several times per second.
    * Make sure the bar is filled with a smooth vertical true-color gradient — green at the bottom, through yellow and orange, to red at the top — and that its top edge uses partial unicode block elements (▁▂▃…█). The bar grows upward as CPU load increases.
    * Make sure that resizing your terminal window causes the chart to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Scrollbar modes`.
    * Make sure a cyan instruction header appears above the panels.
    * Make sure you see four side-by-side panels labeled "Always", "Thumb Only", "While Scrolling", and "Hidden", each containing 50 lines of numbered text.
    * Make sure the "Always" panel shows a gutter (░) with a solid thumb (█) on the right side.
    * Make sure the "Thumb Only" panel shows just the thumb (█), no gutter.
    * Make sure the "While Scrolling" panel shows a thumb only while you are actively scrolling (arrow keys or mouse wheel); it should disappear when you press a non-scroll key.
    * Make sure the "Hidden" panel has no scrollbar at all and its text uses the full width.
    * Make sure that resizing your terminal window causes the panels to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Cursor`.
    * Make sure a cyan instruction header appears at the top of the panel.
    * Make sure a real terminal cursor (not the reverse-video fake) appears at position 0,0 inside the panel.
    * Make sure the arrow keys move the terminal cursor around; it should track the displayed position.
    * Make sure pressing <kbd>s</kbd> cycles the cursor shape through `CursorStyleBlock`, `CursorStyleUnderline`, and `CursorStyleBar` (your terminal must support DECSCUSR).
    * Make sure pressing <kbd>v</kbd> toggles cursor visibility on and off.
    * Type <kbd>Esc</kbd>.

  * Click on `Showcase`. This is one scrollable screen with a header line and two columns; scroll it with the mouse wheel or arrow keys.
    * Make sure the header line shows the current theme name, your terminal's detected `ColorProfile` (e.g. `ColorProfile_TrueColor`), a focus indicator (● focused / ○ unfocused), and the last-known mouse position. The focus indicator should flip when you switch to another terminal window and back; the mouse position should update as you move the mouse over the pane.
    * In the left column, make sure you see labeled samples demonstrating every border preset (single, rounded, thick, double, ascii), padding and margin, foreground/background colors (including an RGB true-color swatch), color operations (darken, lighten, mix, complementary), a 1D gradient swatch (red→green→blue) that fills the width of its row and transitions smoothly, a canvas overlay (a bordered box floating over background text that shows through around it), text transforms (bold, italic, underline, reverse), horizontal alignment within a fixed-width box (left/center/right), a combined rounded-border + padded + colored box, and a hyperlink (underline + OSC 8 clickable in supporting terminals).
    * In the right column, make sure you see themed widgets (a button, a checkbox, a link, and a text input) and a row of color-profile swatches showing the same RGB orange (200,100,50) rendered through each profile: TrueColor shows the exact orange, Ansi256 a close approximation, Ansi16 the nearest ANSI color, and Ascii/NoTTY plain text with no color.
    * Make sure pressing <kbd>Tab</kbd> cycles through the seven predefined themes (`default`, `dark`, `charm`, `dracula`, `nord`, `zenburn`, `gruvbox`), updating the header label and the colors of every element each time. The checkbox should still toggle, and the text input should accept typing in every theme.
    * Make sure that resizing your terminal window causes the layout to resize as well.
    * Type <kbd>Esc</kbd>.

  * Type <kbd>Ctrl</kbd>+<kbd>C</kbd> (or send SIGTERM / `kill <pid>`) to quit.

  * Make sure that after quitting, the terminal restores the content that was on screen before the app launched (alternate screen mode).

There should be 8 examples to cover (Todo List, Text Editor, Scrollable text display, Clickable buttons, CPU Usage, Scrollbar modes, Cursor, Showcase). If there are more or fewer than this then this QA guide is out of date and QA should fail.
