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
    * Make sure that resizing your terminal window causes the boxes to resize as well.
    * Type <kbd>Esc</kbd>.

  * Type <kbd>Ctrl</kbd>+<kbd>C</kbd> to quit.

There should be only 3 examples to cover. If there are more or less than this then this QA guide is out of date and QA should fail.
