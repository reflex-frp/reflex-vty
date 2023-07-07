# Revision history for reflex-vty

## 0.5.1.0

* Change `inputInFocusedRegion` to filter mouse scroll wheel input based on if the region under than the mouse rather than using mouse drag tracking
* Add MonadCatch, MonadThrow, and MonadMask instances (relies on reflex-0.9.2.0 or greater)

## 0.5.0.0

* *Breaking change*:
  * `scrollableText` now takes a `ScrollableConfiguration` instead of just an `Event t Int` of lines to scroll by. Replacing a `scrollableText myEvent` invocation with `scrollableText (def { _scrollableConfig_scrollBy = myEvent })` should recover the old behavior.
  * `scrollableText` now returns a `Scrollable t` instead of a `Behavior (Int, Int)`. The second `Int`, representing the total number of lines, is part of the `Scrollable` record. The first `Int`, representing the current scroll position is replaced by the `ScrollPos` in the `Scrollable` output. `ScrollPos` is a new type that captures whether a `scrollableText` is scrolled to the very top, very bottom, or somewhere in between.
* `scrollableText` can now be given a starting scroll position and an event that scrolls it to a particular position.
* `scrollableText` can be configured to remain scrolled to the bottom on new output, either always or whenever the user is scrolled to the bottom and new output appears.
* Added a new `scrollable` widget in `Reflex.Vty.Widget.Scroll` that allows vertical scrolling when an `Image` is taller than the widget's height.
* Add `ctrlc`, a convenience function that returns an event that fires when a Ctrl+c keypress is detected
* Fix several issues with wide chars, cursor position and word wrapping in Zipper.hs
* Add `centerText` function to Reflex.Vty.Widget.Box

## 0.4.1.1

* Support ghc-9.6

## 0.4.1.0

* Loosen version bounds and support GHC 9.4.4
* Add `MonadHold t (Performable m)` and `MonadFix (Performable m)` instances to `MonadVtyApp`

## 0.4.0.0

* _Breaking Changes_:
  * Added mouse tracking to the behavior of `pane` such that
    * Mouse actions that start outside of the region are not tracked
    * Mouse drag sequences that start OFF the region are NOT reported
    * Mouse drag sequences that start ON the region and drag off ARE reported
    * Introduce `MonadHold` constraint to `pane`
  * Added `MonadHold` constraint to several methods that use `pane`

## 0.3.1.1

* Loosen version bounds and support GHC 9.4

## 0.3.1.0

* Replace `mempty` with `defAttr` for Attr from Graphics.Vty to make it compatible with vty-5.34

## 0.3.0.0

* Re-design `textInput`, `TextInput` and `TextInputConfig`.
  * Allows users to implement more complex behavior.
  * `_textInputConfig_modify` is now applied to the text-value of `textInput`
    after user input events such as mouse clicks and keyboard input.
    This may change the observable behavior.

## 0.2.0.1

* Loosen version bounds on ref-tf and vty

## 0.2.0.0

* _Module Reorganization_: The following modules have been added (and are all re-exported by Reflex.Vty):
  * Reflex.Vty.Widget.Box for all the box functions and datatypes
  * Reflex.Vty.Widget.Input.Mouse for clicking, dragging, and scrolling
  * Reflex.Vty.Widget.Split contains `splitV`, `splitH`, etc
  * Reflex.Vty.Widget.Text contains text rendering functions like `text` and `display`
* _Bugfixes_:
  * Remove text-icu dependency and switch to `wcwidth` from vty package to compute character width in `Data.Text.Zipper`.
  * `goToDisplayLinePosition` in `Data.Text.Zipper` correctly accounts for character width now.
  * [#37](https://github.com/reflex-frp/reflex-vty/issues/37) `Layout` should support focus changes through nested layouts (thanks @pdlla for getting this started -- see entry on Layout and Focus below).
  * Fix distribution of available space when it cannot be evenly distributed. Previously, all leftover space would be allocated to the first stretchable widget.
* _Breaking Changes_:
  * Layout and focus have been substantially refactored to fix [#37](https://github.com/reflex-frp/reflex-vty/issues/37) and support a wider variety of layouts and focus switching requirements.
    * Added a new `HasFocus` class (the old one is now `HasFocusReader`) to produce focusable elements, and manage focus state. See the "Focus" section of the Reflex.Vty.Widget.Layout module documentation.
    * `Layout` no longer has any focus-tracking responsibility. See the "Layout" section of the Reflex.Vty.Widget.Layout module documentation.
    * `tile` no longer takes a configuration record and no longer requires that its child widget return a focus request event. Focus requests are instead handled using calls to `requestFocus` in the child widget.
    * Calls to `fixed` and `stretch` must now be replaced with `tile . fixed` and `tile . stretch`
    * `stretch` now takes a minimum size argument
    * Added `flex` which is equivalent to `stretch 0`
    * `tabNavigation` no longer returns an `Event`. Instead it calls `requestFocus` directly with the appropriate `Refocus_Shift` value.
    * Added `axis` (in `HasLayout`), a lower-level primitive which is used to implement `row` and `col`.
    * Added `region` (in `HasLayout`), which is used to claim screen real estate and used to implement `tile` and `grout`
    * Added `grout`, a container element that is not itself focusable (though its children can be)
  * Removed `VtyWidget` and replaced it with a number of separate classes and monad transformers
    * Replace `HasDisplaySize` with `HasDisplayRegion` which carries around a region instead of just a width and height. `displayWidth` and `displayHeight` are now functions implemented in terms of `askRegion` instead of class methods.
    * Add a `DisplayRegion` monad transformer
    * Rename `ImageWriter` to `HasImageWriter`
    * Introduce an `ImageWriter` monad transformer
    * Rename `HasFocus` to `HasFocusReader`
    * Introduce a `FocusReader` monad transformer
    * Replace `HasVtyInput` with `HasInput`
    * Introduce an `Input` monad transformer
    * Introduce `HasTheme` reader class to allow setting Vty attributes of all built-in widgets
    * Introduce `ThemeReader` monad transformer
  * Remove `DynRegion` and `currentRegion`. Use `Dynamic t Region`  and `current` instead. This also changes the type of `pane`'s argument.
  * `CheckboxConfig` now has a field taking an `Event` to set the value of the checkbox.
  * `checkbox` now accepts keyboard input (spacebar to check and uncheck) and is displayed in bold when focused.
  * `HasInput` (formerly `HasVtyInput`) now has a method `localInput` for filtering the input a child widget may receive
  * `HasImageWriter` now has a method `mapImages` for transforming the images emitted by a child widget
  * `boxTitle` now takes a `Behavior t Text` as its title, instead of a plain `Text`
  * `fill` now takes a `Behavior t Char` instead of a `Char`
  * The following functions are no longer specialized to `VtyWidget`:
    * `pane`: Now requires `HasInput t m, HasImageWriter t m, HasDisplayRegion t m, HasFocusReader t m`
    * `drag`: Now requires `HasInput`
    * `mouseDown`: Now requires `HasInput`
    * `mouseUp`: Now requires `HasInput`
    * `mouseScroll`: Now requires `HasInput`
    * `key`: Now requires `HasInput`
    * `keys`: Now requires `HasInput`
    * `keyCombo`: Now requires `HasInput`
    * `keyCombos`: Now requires `HasInput`
    * `splitV`: Now requires `HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m`
    * `splitH`: Now requires `HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m`
    * `splitVDrag`: Now requires `HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m`
    * `fill`: Now requires `HasImageWriter` and `HasDisplayRegion`
    * `boxTitle`: Now requires `HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasFocusReader t m, HasTheme t m`
    * `box`: Now requires `HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasFocusReader t m, HasTheme t m`
    * `boxStatic`: Now requires `HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasFocusReader t m, HasTheme t m`
    * `richText`: Now requires `HasImageWriter`, and `HasDisplayRegion`
    * `scrollableText`: Now requires `HasInput`, `HasImageWriter`, `HasTheme`, and `HasDisplayRegion`
    * `blank`: Now requires `Monad`
    * `button`: Now requires `HasFocusReader`, `HasInput`, `HasImageWriter`, `HasTheme`, and `HasDisplayRegion`
    * `textButton`: Now requires `HasFocusReader`, `HasInput`, `HasImageWriter`, `HasTheme`, and `HasDisplayRegion`
    * `textButtonStatic`: Now requires `HasFocusReader`, `HasInput`, `HasImageWriter`, `HasTheme`, and `HasDisplayRegion`
    * `link`: Now requires `HasInput`, `HasImageWriter`, `HasTheme`, and `HasDisplayRegion`
    * `checkbox`: Now requires `HasFocusReader`, `HasInput`, `HasImageWriter`, and `HasDisplayRegion`
  * TextZipper interface changes
    * `_displayLines_offsetMap` type changed to `OffsetMapWithAlignment`
    * `_displayLines_cursorY` replaced with `_displayLines_cursorPos` which include X position
    * some exposed methods intended for internal use only have been removed
    * `textInput`: Now requires `HasFocusReader`, `HasInput`, `HasImageWriter`, `HasTheme`, and `HasDisplayRegion`
    * `multilineTextInput`: Now requires `HasFocusReader`, `HasInput`, `HasImageWriter`, `HasTheme`, and `HasDisplayRegion`
    * `textInputTile`: Now requires `HasFocusReader`, `HasInput`, `HasLayout`, `HasTheme`, and `HasFocus`
* _Misc_:
  * (#40 Add alignment support to TextZipper)[https://github.com/reflex-frp/reflex-vty/pull/40]
    * Add alignment (left/center/right) support to TextZipper
    * Add basic unit tests for newly created alignment methods in TextZipper
  * Add default instances for `HasInput`, `HasFocus`, and `HasImageWriter`
  * Export `withinImage` and add `imagesInRegion` to crop images to a region
  * Add `anyChildFocused`, which provides information about whether subwidgets are focused
  * Add `filterKeys`, which is the same as `localInput` but only cares about keyboard events
  * Add `hoistRunLayout` to apply a transformation to the context of a `Layout` action and run that action
  * Add various `MFunctor` instances
  * Add a CPU usage indicator to the example executable

## 0.1.4.2

* Wider bounds for GHC 8.10 support

## 0.1.4.1

## 0.1.4.1
* Migrate to new dependent-sum / dependent-map (after the "some" package split)

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
