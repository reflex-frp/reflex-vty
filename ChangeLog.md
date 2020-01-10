# Revision history for reflex-vty

## Unreleased

* Add `keyCombo` function (single-key-combination version of `keyCombos`)
* Add Reflex.Process module for interacting with shell commands using `Event`s
* Use upstream `NotReady` instances instead of orphans defined in this package if reflex-0.6.3 is available
* Add a sample "ghcid" application
* Add `mouseScroll` to capture scroll wheel events
* Add `scrollableText`: a text display widget that can be scrolled using the mouse or keyboard
* Add widget to the example executable that displays scrollable text

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
