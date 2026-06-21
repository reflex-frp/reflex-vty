# QA Guide

This QA guide takes you through the process of testing `reflex-vty` from a visual observer's perspective. The goal is to ensure that `reflex-vty` does not contain bugs/regressions that are visual in nature, which are hard to detect with automated testing. This guide assumes you have Nix installed.

## Steps

Clone and examples

```bash
nix-shell -p git
cd $(mktemp -d)
git clone https://github.com/reflex-frp/reflex-vty.git --branch develop
nix run -f reflex-vty/release.nix reflex-vty --command example
```

The build may take a while to complete.

Once it's done you can test the following:

  * Make sure that resizing your terminal window causes the editor to resize as well.

  * Click on `Todo List`.
    * Make sure you can check tasks, uncheck tasks, add new tasks, and edit the text of tasks.
    * Make sure that resizing your terminal window causes the editor to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Text Editor`.
    * Make sure you can edit text in the "Text Edit" box. Try entering lots of text so that it doesn't all fit. It should scroll down as you type. Using they arrow keys on your keyboard should move the cursor and let you go back up and see text that was out of view before.
    * Make sure that clicking, right-clicking, using the scroll-wheel on your mouse, and dragging in the "Text Edit" box updates the message in the box lying above and behind the "Text Edit" box.
    * Make sure that dragging your mouse cursor around inside the lowest box causes the message to update. This should update only when dragging *inside* that box, not outside of it.
    * Make sure that resizing your terminal window causes the boxes to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Scrollable text display`.
    * Make sure that you can scroll up and down through the small box of text with your mouse's scroll wheel.
    * Make sure that the second box, "This one scrolls automatically as the output grows", appends one timestamped line per second and stays scrolled to the bottom as new lines appear.
    * Make sure that the "Height", "Scroll", and "Length" readouts below update as you scroll and as new lines arrive.
    * Make sure that resizing your terminal window causes the boxes to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Clickable buttons`.
    * Make sure that clicking each of the nine buttons (or focusing one and pressing <kbd>Enter</kbd> or <kbd>Space</kbd>) appends its associated emoji into the "CLICK BUTTONS TO DRAW" box.
    * Make sure the focused button is rendered with a double-line border; unfocused buttons use the default single-line border.
    * Type <kbd>Esc</kbd>.

  * Click on `CPU Usage`.
    * Make sure a vertical bar chart appears inside the titled box and updates several times per second.
    * Make sure the bars are colored (red/orange/yellow/white by load) and use unicode block elements.
    * Make sure that resizing your terminal window causes the chart to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Scrollable`.
    * Make sure the titled "Tracks" box contains 11 numbered, focusable buttons stacked vertically.
    * Make sure you can scroll through them with the mouse wheel or arrow keys, and that focus tracks the visible buttons.
    * Make sure the "Total Lines", "Scroll Pos", and "Scroll Height" readouts below the box update as you scroll.
    * Type <kbd>Esc</kbd>.

  * Type <kbd>Ctrl</kbd>+<kbd>C</kbd> to quit.

There should be 6 examples to cover. If there are more or less than this then this QA guide is out of date and QA should fail.
