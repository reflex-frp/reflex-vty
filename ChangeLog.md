# Revision history for reflex-vty

## 0.2.0.0
* Remove text-icu dependency and switch to `wcwidth` from vty package to compute character width in `Data.Text.Zipper`.
* `goToDisplayLinePosition` in `Data.Text.Zipper` correctly accounts for character width now.
* [#37](https://github.com/reflex-frp/reflex-vty/issues/37) `Layout` should support focus changes through nested layouts (thanks @pdlla for getting this started).
* _Breaking Change_: Layout and focus have been substantially refactored:
  * Added `MonadFocus` to produce focusable elements, and manage focus state. See the "Focus" section of the Reflex.Vty.Widget.Layout module documentation.
  * `Layout` no longer has any focus-tracking responsibility. See the "Layout" section of the Reflex.Vty.Widget.Layout module documentation.
  * `tile` no longer takes a configuration record and no longer requires that its child widget return a focus request event. Focus requests are instead handled using calls to `requestFocus` in the child widget.
  * Calls to `fixed` and `stretch` must now be replaced with `tile . fixed` and `tile . stretch`
  * `stretch` now takes a minimum size argument
  * Added `flex` which is equivalent to `stretch 0`
  * `tabNavigation` no longer returns an `Event`. Instead it calls `requestFocus` directly with the appropriate `Refocus_Shift` value.
  * Added `axis` (in `MonadLayout`), a lower-level primitive which is used to implement `row` and `col`.
  * Added `region` (in `MonadLayout`), which is used to claim screen real estate and used to implement `tile` and `grout`
  * Added `grout`, a container element that is not itself focusable (though its children can be)
* Add `HasVtyWidgetCtx` for `pane`-like sub-widget container behavior
* Add default instances for `HasVtyInput`, `HasFocus`, and `ImageWriter`
* _Breaking Change_: Remove `DynRegion` and `currentRegion`. Use `Dynamic t Region`  and `current` instead. This also changes the type of `pane`'s argument.
* _Breaking Change_: The following functions are no longer specialized to `VtyWidget`:
  * `pane`: Now requires `HasVtyWidgetCtx` instead
  * `drag`: Now requires `HasVtyInput` instead
  * `mouseDown`: Now requires `HasVtyInput` instead
  * `mouseUp`: Now requires `HasVtyInput` instead
  * `mouseScroll`: Now requires `HasVtyInput` instead
  * `key`: Now requires `HasVtyInput` instead
  * `keys`: Now requires `HasVtyInput` instead
  * `keyCombo`: Now requires `HasVtyInput` instead
  * `keyCombos`: Now requires `HasVtyInput` instead
  * `splitV`: Now requires `HasVtyWidgetCtx` and `HasDisplaySize` instead
  * `splitH`: Now requires `HasVtyWidgetCtx` and `HasDisplaySize` instead
  * `splitVDrag`: Now requires `HasVtyWidgetCtx`, `HasVtyInput`,  and `HasDisplaySize` instead
  * `fill`: Now requires `ImageWriter` and `HasDisplaySize`  instead
  * `boxTitle`: Now requires `HasVtyWidgetCtx`, `ImageWriter`, and `HasDisplaySize`  instead
  * `box`: Now requires `HasVtyWidgetCtx`, `ImageWriter`, and `HasDisplaySize`  instead
  * `boxStatic`: Now requires `HasVtyWidgetCtx`, `ImageWriter`, and `HasDisplaySize`  instead
  * `richText`: Now requires `ImageWriter`, and `HasDisplaySize`  instead
  * `scrollableText`: Now requires `HasVtyInput`, `ImageWriter`, and `HasDisplaySize`  instead
  * `blank`: Now required `Monad` instead
  * `button`: Now requires `HasFocus`, `HasVtyWidgetCtx`, `HasVtyInput`, `ImageWriter`, and `HasDisplaySize`  instead
  * `textButton`: Now requires `HasFocus`, `HasVtyWidgetCtx`, `HasVtyInput`, `ImageWriter`, and `HasDisplaySize`  instead
  * `textButtonStatic`: Now requires `HasFocus`, `HasVtyWidgetCtx`, `HasVtyInput`, `ImageWriter`, and `HasDisplaySize`  instead
  * `link`: Now requires `HasVtyInput`, `ImageWriter`, and `HasDisplaySize`  instead
  * `checkbox`: Now requires `HasFocus`, `HasVtyInput`, `ImageWriter`, and `HasDisplaySize`  instead
  * `textInput`: Now requires `HasFocus`, `HasVtyInput`, `ImageWriter`, and `HasDisplaySize`  instead
  * `multilineTextInput`: Now requires `HasFocus`, `HasVtyInput`, `ImageWriter`, and `HasDisplaySize`  instead
  * `textInputTile`: Now requires `HasVtyWidgetCtx`, `HasVtyInput`, `MonadLayout`, and `MonadFocus` instead
* _Breaking Change_: `CheckboxConfig` now has a field taking an `Event` to set the value of the checkbox.
* _Breaking Change_: `checkbox` now accepts keyboard input (spacebar to check and uncheck) and is displayed in bold when focused.

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
