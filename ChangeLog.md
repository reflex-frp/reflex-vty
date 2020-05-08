# Revision history for reflex-vty

## 0.1.4.0
* ([#15](https://github.com/reflex-frp/reflex-vty/pull/15)) Add `PostBuild` instance for `Layout`.
* ([#17](https://github.com/reflex-frp/reflex-vty/pull/17)) Add `splitH` to implement horizontal functionality of `splitV`.
* ([#19](https://github.com/reflex-frp/reflex-vty/pull/19)) Add `boxTitle`: a box with a title.
* ([#19](https://github.com/reflex-frp/reflex-vty/pull/19)) Update the text editing example to use `boxTitle`.
* ([#21](https://github.com/reflex-frp/reflex-vty/pull/21)) Fix bug in `drag` that caused dragging with different mouse button to trigger the click event.
* ([#22](https://github.com/reflex-frp/reflex-vty/pull/22)) Add support for GHC 8.8.

## 0.1.3.0
* Add `mouseScroll` to capture scroll wheel events.
* Add `scrollableText`: a text display widget that can be scrolled using the mouse or keyboard.
* Add widget to the example executable that displays scrollable text.

## 0.1.2.1
* Add `keyCombo` function (single-key-combination version of `keyCombos`).
* Use upstream `NotReady` instances instead of orphans defined in this package if reflex-0.6.3 is available.

## 0.1.2.0
* Allow `TextZipper` contents to be transformed before being displayed.
* Fix bug in `row` orientation.
* Handle wrapping of lines containing full-width unicode characters in `textInput`.

## 0.1.1.1
* Bump minimum version of reflex.

## 0.1.1.0
* Set version bounds in cabal file.
* Add travis CI config.

## 0.1.0.0
* Initial release
