# Revision history for reflex-vty

## 0.1.3.1
* Add `splitH` to implement horizontal functionality of `splitV`.
* Add `boxTitle`: a box with a title
* Update the text editing example to use `boxTitle`

## 0.1.3.0
* Add `mouseScroll` to capture scroll wheel events
* Add `scrollableText`: a text display widget that can be scrolled using the mouse or keyboard
* Add widget to the example executable that displays scrollable text

## 0.1.2.1
* Add `keyCombo` function (single-key-combination version of `keyCombos`)
* Use upstream `NotReady` instances instead of orphans defined in this package if reflex-0.6.3 is available

## 0.1.2.0
* Allow TextZipper contents to be tranformed before being displayed
* Fix bug in `row` orientation
* Handle wrapping of lines containing fullwidth unicode characters in `textInput`

## 0.1.1.1

* Bump minimum version of reflex

## 0.1.1.0

* Set version bounds in cabal file
* Add travis CI config

## 0.1.0.0

* Initial release
