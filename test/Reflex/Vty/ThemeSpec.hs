{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Vty.ThemeSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Reflex.Vty.Host
import Reflex.Vty.Style (Style, applyAttr, def, withForeground)
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), attrAt)
import Reflex.Vty.Theme
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Box (boxStatic, singleBoxStyle)
import Reflex.Vty.Widget.Input.Text (TextInput (..), textInput)
import Reflex.Vty.Widget.Split (splitH)
import Reflex.Vty.Widget.Text (fill)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Text content of one row of a rendered grid.
rowText :: Grid -> Int -> String
rowText g y = T.unpack . T.concat . map renderSpanText $ gridRows g !! y

-- | The terminal default attribute (the starting point of resolution).
defaultAttr :: V.FixedAttr
defaultAttr = V.FixedAttr V.defaultStyleMask Nothing Nothing Nothing

-- | Effective foreground color at a cell, if any.
foreAt :: Int -> Int -> Grid -> Maybe (Maybe V.Color)
foreAt x y g = V.fixedForeColor <$> attrAt x y g

themeWithForeground :: V.Color -> Theme
themeWithForeground c = defTheme {_theme_default = withForeground c def}

-- | Magenta on one side, green on the other; the command switches the
-- first theme to blue.
magenta, blue, green :: V.Color
magenta = V.rgbColor 255 0 255
blue = V.rgbColor 0 0 255
green = V.rgbColor 0 255 0

----------------------------------------------------------------------------
-- Applications under test
----------------------------------------------------------------------------

-- | Two boxed text inputs under different local themes. Only the first
-- input is focused, so only it consumes key events. The command changes
-- the first pane's theme from magenta to blue; the second pane's theme
-- never changes.
twoThemeApp :: TestApp Bool (T.Text, T.Text)
twoThemeApp cmd = do
  theme1 <- holdDyn (themeWithForeground magenta) (themeWithForeground blue <$ cmd)
  (ti1, ti2) <-
    splitH
      (pure (`div` 2))
      (pure (True, False))
      (localTheme (\_ -> current theme1) (boxedInput))
      (localTheme (\_ -> pure (themeWithForeground green)) (boxedInput))
  pure ((,) <$> _textInput_value ti1 <*> _textInput_value ti2, never)
  where
    boxedInput = boxStatic singleBoxStyle (textInput def)

-- | One preset-themed widget: a filled box under the Dracula theme.
presetApp :: Theme -> TestApp () ()
presetApp th _cmd = do
  _ <- localTheme (\_ -> pure th) (boxStatic singleBoxStyle (fill (pure '.')))
  pure (pure (), never)

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

spec :: Spec
spec = describe "Reflex.Vty.Theme" $ do
  describe "Theme record and themeAttr" $ do
    it "defTheme's title style adds bold over any base attribute" $
      applyAttr (_theme_title defTheme) V.defAttr `shouldBe` V.withStyle V.defAttr V.bold

    it "themeAttr applies the default style to the terminal default" $ do
      -- The Behavior is pure. The initial sample gives the full result.
      let attr0 = _theme_default (themeWithForeground magenta)
      V.attrForeColor (applyAttr attr0 V.defAttr) `shouldBe` V.SetTo magenta

  describe "localTheme through the production host" $ do
    it "updates only the scoped subtree, in the frame that contains the command" $
      withApp defaultVtyAppConfig (20, 5) twoThemeApp $ \driver -> do
        -- Type into the focused left input.
        _ <- stepInput driver (V.EvKey (V.KChar 'h') [])
        fTyped <- stepInput driver (V.EvKey (V.KChar 'i') [])
        let gTyped = frameGrid fTyped
        -- Left border and text are magenta; right border is green.
        foreAt 0 0 gTyped `shouldBe` Just (Just magenta)
        foreAt 1 1 gTyped `shouldBe` Just (Just magenta)
        foreAt 10 0 gTyped `shouldBe` Just (Just green)
        rowText gTyped 1 `shouldSatisfy` isInfixOf "hi"
        frameObservation fTyped `shouldBe` ("hi", "")
        -- Switching only the first theme recolors exactly the first
        -- subtree in this very frame: no extra keypress involved.
        fSwitched <- stepCommand driver True
        let gSwitched = frameGrid fSwitched
        foreAt 0 0 gSwitched `shouldBe` Just (Just blue)
        foreAt 1 1 gSwitched `shouldBe` Just (Just blue)
        -- The sibling subtree is untouched.
        foreAt 10 0 gSwitched `shouldBe` Just (Just green)
        -- Both input values are intact.
        rowText gSwitched 1 `shouldSatisfy` isInfixOf "hi"
        frameObservation fSwitched `shouldBe` ("hi", "")
        stopApp driver

    it "isolates sibling subtrees with constant local themes" $
      withApp defaultVtyAppConfig (20, 5) twoThemeApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            let g = frameGrid f
            foreAt 0 0 g `shouldBe` Just (Just magenta)
            foreAt 10 0 g `shouldBe` Just (Just green)
            -- Both inputs start empty and stay distinct.
            frameObservation f `shouldBe` ("", "")
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

  describe "preset themes" $ do
    it "renders a dracula-themed widget with non-default attributes" $
      withApp defaultVtyAppConfig (20, 5) (presetApp draculaTheme) $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            let g = frameGrid f
            attrAt 0 0 g `shouldNotBe` Just defaultAttr
            foreAt 0 0 g `shouldNotBe` Nothing
            rowText g 0 `shouldSatisfy` isInfixOf "┌"
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

    it "renders a dark-themed widget with explicit foreground and background" $
      withApp defaultVtyAppConfig (20, 5) (presetApp darkTheme) $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            let g = frameGrid f
            -- darkTheme sets both a foreground and a background color
            -- without pinning the exact palette constants.
            attrAt 0 0 g `shouldNotBe` Just defaultAttr
            foreAt 0 0 g `shouldNotBe` Nothing
            foreAt 0 0 g `shouldNotBe` Just Nothing
            (attrAt 0 0 g >>= V.fixedBackColor) `shouldNotBe` Nothing
          [] -> expectationFailure "expected an initial frame"
        stopApp driver
