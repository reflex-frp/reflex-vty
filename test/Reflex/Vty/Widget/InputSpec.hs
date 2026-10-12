{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: Behavior-covering specs for the input widgets in
-- "Reflex.Vty.Widget.Input", driven through the production host.
module Reflex.Vty.Widget.InputSpec (spec) where

import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Reflex.Vty.Host
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), attrAt)
import Reflex.Vty.Widget (input, localInput)
import Reflex.Vty.Widget.Input
import Reflex.Vty.Widget.Input.Mouse
import Reflex.Vty.Widget.Layout (fixed, initManager_, row, tabNavigation, tile)

-- | Terminal-default attribute: the baseline against which focus and
-- depressed styling is asserted on the rendered grid.
defaultAttr :: V.FixedAttr
defaultAttr = V.FixedAttr V.defaultStyleMask Nothing Nothing Nothing

-- | The full text of one rendered row of a frame.
frameRow :: Frame o -> Int -> String
frameRow f y = case drop y (gridRows (frameGrid f)) of
  (r : _) -> T.unpack (T.concat (map renderSpanText r))
  [] -> ""

-- | The newest frame drawn so far (for a static app, the initial frame).
lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame dr = last <$> driverFrames dr

-- | Press and release the left button at screen coordinates.
clickAt :: AppDriver c o -> (Int, Int) -> IO (Frame o)
clickAt dr (x, y) = do
  _ <- stepInput dr (V.EvMouseDown x y V.BLeft [])
  stepInput dr (V.EvMouseUp x y (Just V.BLeft))

-- | Count occurrences of an event.
countEvents :: (Reflex t, MonadHold t m, MonadFix m) => Event t () -> m (Dynamic t Int)
countEvents e = foldDyn (\_ n -> n + 1) 0 e

-- | Expected screen row for two side-by-side 6-wide checkbox tiles on a
-- 20-wide terminal: the checkbox glyphs plus tile and screen padding.
checkboxRow :: Char -> Char -> String
checkboxRow a b =
  concat
    [ "["
    , [a]
    , "]"
    , replicate 3 ' '
    , "["
    , [b]
    , "]"
    , replicate 11 ' '
    ]

----------------------------------------------------------------------------
-- Applications under test
----------------------------------------------------------------------------

-- | Two 10x3 buttons side by side in focusable tiles. Observation: click
-- counts of the left and right button.
buttonApp :: TestApp () (Int, Int)
buttonApp _cmd = do
  (clickA, clickB) <- initManager_ $ do
    tabNavigation
    row $ do
      a <- tile (fixed (pure 10)) $ textButtonStatic def "OK"
      b <- tile (fixed (pure 10)) $ textButtonStatic def "NO"
      pure (a, b)
  countA <- countEvents clickA
  countB <- countEvents clickB
  pure (zipDynWith (,) countA countB, never)

-- | Two 6x1 checkboxes in focusable tiles. The command sets the value of
-- the left checkbox. Observation: (left, right) values.
checkboxApp :: TestApp () (Bool, Bool)
checkboxApp cmd = do
  (a, b) <- initManager_ $ do
    tabNavigation
    row $ do
      ca <- tile (fixed (pure 6)) $ checkbox (CheckboxConfig (pure checkboxStyleX) (False <$ cmd)) False
      cb <- tile (fixed (pure 6)) $ checkbox def False
      pure (ca, cb)
  pure (zipDynWith (,) a b, never)

-- | One checkbox that, on every command, simultaneously receives a
-- synthetic space keypress (through its input) and a @setValue True@.
-- Both are derived from the same command event, so they are
-- simultaneous; the UI toggle takes precedence. The checkbox starts
-- checked so the two outcomes are distinguishable.
precedenceApp :: TestApp () Bool
precedenceApp cmd = do
  v <-
    localInput (\orig -> leftmost [orig, V.EvKey (V.KChar ' ') [] <$ cmd]) $
      checkbox (CheckboxConfig (pure checkboxStyleX) (True <$ cmd)) True
  pure (v, never)

-- | A link with an 8-wide focusable tile. Observation: activation count
-- and the region-relative coordinates of the last mouse-up.
linkApp :: TestApp () (Int, (Int, Int))
linkApp _cmd = do
  clicks <-
    initManager_ . row . tile (fixed (pure 8)) $ do
      lu <- linkStatic "site"
      foldDyn (\u (n, _) -> (n + 1, _mouseUp_coordinates u)) (0, (0, 0)) lu
  pure (clicks, never)

----------------------------------------------------------------------------
-- Specs
----------------------------------------------------------------------------

spec :: Spec
spec = describe "Reflex.Vty.Widget.Input" $ do
  describe "button" $ do
    it "ignores keyboard activation while unfocused and activates on Enter once focused" $
      withApp defaultVtyAppConfig (24, 3) buttonApp $ \dr -> do
        f0 <- lastFrame dr
        take 20 (frameRow f0 0) `shouldBe` "┌────────┐┌────────┐"
        frameObservation f0 `shouldBe` (0, 0)
        fEnterUnfocused <- stepInput dr (V.EvKey V.KEnter [])
        frameObservation fEnterUnfocused `shouldBe` (0, 0)
        fTab <- stepInput dr (V.EvKey (V.KChar '\t') [])
        take 20 (frameRow fTab 0) `shouldBe` "┏━━━━━━━━┓┌────────┐"
        fEnter <- stepInput dr (V.EvKey V.KEnter [])
        frameObservation fEnter `shouldBe` (1, 0)
        fSpace <- stepInput dr (V.EvKey (V.KChar ' ') [])
        frameObservation fSpace `shouldBe` (1, 0)
        stopApp dr

    it "activates on a mouse click inside the button region and moves focus to the clicked button" $
      withApp defaultVtyAppConfig (24, 3) buttonApp $ \dr -> do
        _ <- stepInput dr (V.EvKey (V.KChar '\t') [])
        fLeft <- clickAt dr (5, 1)
        frameObservation fLeft `shouldBe` (1, 0)
        fRight <- clickAt dr (15, 1)
        frameObservation fRight `shouldBe` (1, 1)
        take 20 (frameRow fRight 0) `shouldBe` "┌────────┐┏━━━━━━━━┓"
        stopApp dr

    it "does not activate on a click outside every button region" $
      withApp defaultVtyAppConfig (24, 3) buttonApp $ \dr -> do
        _ <- stepInput dr (V.EvKey (V.KChar '\t') [])
        f0 <- lastFrame dr
        fOff <- clickAt dr (22, 1)
        frameObservation fOff `shouldBe` (0, 0)
        take 20 (frameRow fOff 0) `shouldBe` take 20 (frameRow f0 0)
        stopApp dr

  describe "checkbox" $ do
    it "reflects focus in the rendered attributes and toggles via the space key" $
      withApp defaultVtyAppConfig (20, 1) checkboxApp $ \dr -> do
        f0 <- lastFrame dr
        frameRow f0 0 `shouldBe` checkboxRow ' ' ' '
        frameObservation f0 `shouldBe` (False, False)
        attrAt 0 0 (frameGrid f0) `shouldBe` Just defaultAttr
        attrAt 6 0 (frameGrid f0) `shouldBe` Just defaultAttr
        fTab <- stepInput dr (V.EvKey (V.KChar '\t') [])
        -- The focused checkbox renders with a non-default (bold)
        -- attribute; the unfocused one stays at terminal defaults.
        attrAt 0 0 (frameGrid fTab) `shouldNotBe` Just defaultAttr
        attrAt 6 0 (frameGrid fTab) `shouldBe` Just defaultAttr
        attrAt 0 0 (frameGrid fTab) `shouldNotBe` attrAt 6 0 (frameGrid fTab)
        fSpace <- stepInput dr (V.EvKey (V.KChar ' ') [])
        frameObservation fSpace `shouldBe` (True, False)
        frameRow fSpace 0 `shouldBe` checkboxRow 'x' ' '
        fEnter <- stepInput dr (V.EvKey V.KEnter [])
        frameObservation fEnter `shouldBe` (True, False)
        stopApp dr

    it "toggles via a mouse click, shows depressed styling during the press, and focuses the clicked box" $
      withApp defaultVtyAppConfig (20, 1) checkboxApp $ \dr -> do
        f0 <- lastFrame dr
        frameObservation f0 `shouldBe` (False, False)
        fDown <- stepInput dr (V.EvMouseDown 7 0 V.BLeft [])
        frameObservation fDown `shouldBe` (False, False)
        frameRow fDown 0 `shouldBe` checkboxRow ' ' ' '
        attrAt 7 0 (frameGrid fDown) `shouldNotBe` Just defaultAttr
        fUp <- stepInput dr (V.EvMouseUp 7 0 (Just V.BLeft))
        frameObservation fUp `shouldBe` (False, True)
        frameRow fUp 0 `shouldBe` checkboxRow ' ' '\10003'
        -- Focus followed the click: right box now styled, left default.
        attrAt 6 0 (frameGrid fUp) `shouldNotBe` Just defaultAttr
        attrAt 0 0 (frameGrid fUp) `shouldBe` Just defaultAttr
        stopApp dr

    it "updates its value programmatically via _checkboxConfig_setValue" $
      withApp defaultVtyAppConfig (20, 1) checkboxApp $ \dr -> do
        _ <- stepInput dr (V.EvKey (V.KChar '\t') [])
        fSpace <- stepInput dr (V.EvKey (V.KChar ' ') [])
        frameObservation fSpace `shouldBe` (True, False)
        fSet <- stepCommand dr ()
        frameObservation fSet `shouldBe` (False, False)
        frameRow fSet 0 `shouldBe` checkboxRow ' ' ' '
        stopApp dr

    it "ignores Enter and clicks outside its region" $
      withApp defaultVtyAppConfig (20, 1) checkboxApp $ \dr -> do
        f0 <- lastFrame dr
        fEnter <- stepInput dr (V.EvKey V.KEnter [])
        frameObservation fEnter `shouldBe` (False, False)
        fOff <- clickAt dr (16, 0)
        frameObservation fOff `shouldBe` (False, False)
        frameRow fOff 0 `shouldBe` frameRow f0 0
        stopApp dr

    it "gives a simultaneous UI toggle precedence over _checkboxConfig_setValue" $
      withApp defaultVtyAppConfig (10, 1) precedenceApp $ \dr -> do
        f0 <- lastFrame dr
        frameObservation f0 `shouldBe` True
        take 3 (frameRow f0 0) `shouldBe` "[x]"
        f1 <- stepCommand dr ()
        frameObservation f1 `shouldBe` False
        take 3 (frameRow f1 0) `shouldBe` "[ ]"
        stopApp dr

  describe "link" $ do
    it "renders with link styling and activates on a click inside its region with region-relative coordinates" $
      withApp defaultVtyAppConfig (12, 1) linkApp $ \dr -> do
        f0 <- lastFrame dr
        take 4 (frameRow f0 0) `shouldBe` "site"
        frameObservation f0 `shouldBe` (0, (0, 0))
        attrAt 0 0 (frameGrid f0) `shouldNotBe` Just defaultAttr
        attrAt 0 0 (frameGrid f0) `shouldNotBe` attrAt 5 0 (frameGrid f0)
        fClick <- clickAt dr (3, 0)
        frameObservation fClick `shouldBe` (1, (3, 0))
        take 4 (frameRow fClick 0) `shouldBe` "site"
        stopApp dr

    it "does not activate on keyboard events" $
      withApp defaultVtyAppConfig (12, 1) linkApp $ \dr -> do
        fEnter <- stepInput dr (V.EvKey V.KEnter [])
        frameObservation fEnter `shouldBe` (0, (0, 0))
        fSpace <- stepInput dr (V.EvKey (V.KChar ' ') [])
        frameObservation fSpace `shouldBe` (0, (0, 0))
        stopApp dr

    it "does not activate on a click outside its region" $
      withApp defaultVtyAppConfig (12, 1) linkApp $ \dr -> do
        fClick <- clickAt dr (3, 0)
        frameObservation fClick `shouldBe` (1, (3, 0))
        fOff <- clickAt dr (10, 0)
        frameObservation fOff `shouldBe` (1, (3, 0))
        stopApp dr
