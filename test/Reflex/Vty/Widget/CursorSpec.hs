{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Widget.CursorSpec (spec) where

import qualified Graphics.Vty as V
import Reflex (never)
import Test.Hspec

import Reflex.Vty.Host
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Widget

spec :: Spec
spec = describe "Reflex.Vty.Widget cursor" $ do
  describe "defaultCursorState" $
    it "starts hidden with terminal-default style" $
      defaultCursorState `shouldBe` CursorState CursorHidden CursorStyleDefault (0, 0)

  describe "translateCursorState" $ do
    it "translates visible viewport-relative positions to absolute positions" $
      translateCursorState
        (Region 10 20 5 3)
        (CursorState CursorVisible CursorStyleBar (2, 1))
        `shouldBe` CursorState CursorVisible CursorStyleBar (12, 21)

    it "hides visible cursors outside the viewport" $
      translateCursorState
        (Region 10 20 5 3)
        (CursorState CursorVisible CursorStyleBlock (5, 1))
        `shouldBe` CursorState CursorHidden CursorStyleBlock (15, 21)

    it "translates hidden cursor positions without changing visibility" $
      translateCursorState
        (Region 10 20 5 3)
        (CursorState CursorHidden CursorStyleUnderline (7, 4))
        `shouldBe` CursorState CursorHidden CursorStyleUnderline (17, 24)

  describe "cursorStateToVtyCursor" $ do
    it "maps hidden cursor state to NoCursor" $
      cursorStateToVtyCursor (CursorState CursorHidden CursorStyleBlock (2, 3))
        `shouldBe` V.NoCursor

    it "maps visible cursor state to absolute terminal-cell coordinates" $
      -- reflex-vty coordinates are terminal cells. A visible cursor
      -- maps to 'V.AbsoluteCursor' (cell coordinates). It never maps
      -- to 'V.Cursor' (character offsets).
      cursorStateToVtyCursor (CursorState CursorVisible CursorStyleBlock (2, 3))
        `shouldBe` V.AbsoluteCursor 2 3

  describe "setCursor through the production host" $ do
    it "reports the cursor at the cell position after the wide glyph" $
      withApp defaultVtyAppConfig (20, 3) cursorApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> frameCursor f `shouldBe` V.AbsoluteCursor 3 0
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

  describe "ScreenMode" $ do
    it "has two constructors" $
      [ScreenNormal ..] `shouldBe` [ScreenNormal, ScreenAlternate]

    it "ScreenNormal /= ScreenAlternate" $
      ScreenNormal `shouldNotBe` ScreenAlternate

-- | A static row with one wide glyph. The terminal cursor sits three
-- cells from the left edge.
cursorApp :: TestApp () ()
cursorApp _cmd = do
  tellImages (pure [V.string V.defAttr "a\30028|"])
  setCursor (pure (CursorState CursorVisible CursorStyleDefault (3, 0)))
  pure (pure (), never)
