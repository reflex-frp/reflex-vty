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

  * Click on `Scrollbar modes`.
    * Make sure you see four side-by-side panels labeled "Always", "Thumb Only", "While Scrolling", and "Hidden", each containing 50 lines of numbered text.
    * Make sure the "Always" panel shows a gutter (░) with a solid thumb (█) on the right side.
    * Make sure the "Thumb Only" panel shows just the thumb (█), no gutter.
    * Make sure the "While Scrolling" panel shows a thumb only while you are actively scrolling (arrow keys or mouse wheel); it should disappear when you press a non-scroll key.
    * Make sure the "Hidden" panel has no scrollbar at all and its text uses the full width.
    * Make sure that resizing your terminal window causes the panels to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Cursor`.
    * Make sure a real terminal cursor (not the reverse-video fake) appears at position 0,0 inside the panel.
    * Make sure the arrow keys move the terminal cursor around; it should track the displayed position.
    * Make sure pressing <kbd>s</kbd> cycles the cursor shape through `CursorStyleBlock`, `CursorStyleUnderline`, and `CursorStyleBar` (your terminal must support DECSCUSR).
    * Make sure pressing <kbd>v</kbd> toggles cursor visibility on and off.
    * Type <kbd>Esc</kbd>.

  * Click on `Styles`.
    * Make sure you see a grid of labeled boxes demonstrating every border style preset (single, rounded, thick, double, ascii), padding and margin, foreground/background colors (including an RGB true-color swatch), color operations (darken, lighten, mix, complementary), a 1D gradient swatch (red→green→blue), text transforms (bold, italic, underline, reverse), horizontal alignment within a fixed-width box (left/center/right), a combined rounded-border + padded + colored box, and a hyperlink (underline + OSC 8 clickable in supporting terminals).
    * Make sure the gradient swatch fills the width of its row and smoothly transitions from red through green to blue.
    * Make sure that resizing your terminal window causes the layout to resize as well.
    * Type <kbd>Esc</kbd>.

  * Click on `Color Profile`.
    * Make sure the top line shows your terminal's detected `ColorProfile` (e.g. `ColorProfile_TrueColor` on a modern terminal).
    * Make sure the row of swatches shows the same RGB orange (200,100,50) rendered through each profile: TrueColor shows the exact orange, Ansi256 shows a close approximation, Ansi16 shows the nearest ANSI color, Ascii and NoTTY show plain text with no color.
    * Type <kbd>Esc</kbd>.

  * Click on `Themes`.
    * Make sure you see a titled panel showing a button, a checkbox, a link, and a text input, all rendered in the currently selected theme.
    * Make sure pressing <kbd>Tab</kbd> cycles through the seven predefined themes (`default`, `dark`, `charm`, `dracula`, `nord`, `zenburn`, `gruvbox`), updating the panel's title and the colors of every element each time.
    * Make sure the checkbox still toggles, the link is underlined, and the text input accepts typing in every theme.
    * Type <kbd>Esc</kbd>.

  * Type <kbd>Ctrl</kbd>+<kbd>C</kbd> to quit.

There should be 10 examples to cover. If there are more or less than this then this QA guide is out of date and QA should fail.
