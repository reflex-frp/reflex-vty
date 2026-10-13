{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Vty.Widget.BoxSpec (spec) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Data.Text.Zipper (TextAlignment (..))
import Reflex.Vty.Host
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..))
import Reflex.Vty.Widget (blank, tellImages)
import Reflex.Vty.Widget.Box
import Reflex.Vty.Widget.Input.Mouse
import Reflex.Vty.Widget.Text (text)

-- | The full text of one rendered row of a frame.
frameRow :: Frame o -> Int -> String
frameRow f y = case drop y (gridRows (frameGrid f)) of
  (r : _) -> T.unpack (T.concat (map renderSpanText r))
  [] -> ""

-- | The newest frame drawn so far (for a static app, the initial frame).
lastFrame :: AppDriver c o -> IO (Frame o)
lastFrame dr = last <$> driverFrames dr

----------------------------------------------------------------------------
-- Applications under test
----------------------------------------------------------------------------

-- | A full-screen single-style box containing the text \"hi\".
boxApp :: TestApp () ()
boxApp _cmd = do
  _ <- boxStatic singleBoxStyle (text (pure "hi"))
  pure (pure (), never)

-- | A full-screen titled box whose title alignment is driven by commands.
titleApp :: TestApp TextAlignment ()
titleApp cmd = do
  align <- holdDyn TextAlignment_Center cmd
  _ <- boxTitle (current align) (pure singleBoxStyle) (pure "Hi") blank
  pure (pure (), never)

-- | A full-screen titled box whose title is wider than the box.
clipTitleApp :: TestApp () ()
clipTitleApp _cmd = do
  _ <- boxTitle (pure TextAlignment_Left) (pure singleBoxStyle) (pure "ABCDEFGHIJKLM") blank
  pure (pure (), never)

-- | A full-screen box whose child records the region-relative coordinates
-- of left-button presses.
translateApp :: TestApp () (Int, Int)
translateApp _cmd = do
  pos <-
    box (pure singleBoxStyle) $ do
      md <- mouseDown V.BLeft
      holdDyn (-1, -1) (_mouseDown_coordinates <$> md)
  pure (pos, never)

-- | A full-screen box whose child tries to paint far beyond the box.
clipApp :: TestApp () ()
clipApp _cmd = do
  _ <-
    boxStatic singleBoxStyle $
      tellImages (pure [V.string V.defAttr (replicate 40 'X')])
  pure (pure (), never)

-- | A full-screen horizontal rule.
hRuleApp :: TestApp () ()
hRuleApp _cmd = do
  hRule singleBoxStyle
  pure (pure (), never)

----------------------------------------------------------------------------
-- Specs
----------------------------------------------------------------------------

spec :: Spec
spec = describe "Reflex.Vty.Widget.Box" $ do
  describe "alignText" $ do
    it "left-aligns by padding on the right" $ do
      alignText TextAlignment_Left "hi" '-' 5 `shouldBe` "hi---"

    it "right-aligns by padding on the left" $ do
      alignText TextAlignment_Right "hi" '-' 5 `shouldBe` "---hi"

    it "center-aligns with left bias for odd slack" $ do
      alignText TextAlignment_Center "hi" '-' 5 `shouldBe` "--hi-"

    it "center-aligns evenly for even slack" $ do
      alignText TextAlignment_Center "hi" '-' 6 `shouldBe` "--hi--"

    it "returns text unchanged when it fills the width" $ do
      alignText TextAlignment_Left "hello" '-' 3 `shouldBe` "hello"

    it "returns text unchanged when it exceeds the width" $ do
      alignText TextAlignment_Center "hello" '-' 2 `shouldBe` "hello"

    it "measures wide characters correctly" $ do
      alignText TextAlignment_Left "文" '-' 4 `shouldBe` "文--"

    it "handles empty text" $ do
      alignText TextAlignment_Center "" '-' 3 `shouldBe` "---"

  describe "centerText" $ do
    it "is a specialization of alignText" $ do
      centerText "hi" '-' 5 `shouldBe` alignText TextAlignment_Center "hi" '-' 5

  describe "boxStatic through the production host" $ do
    it "renders borders, inner padding, and the child content" $
      withApp defaultVtyAppConfig (12, 5) boxApp $ \dr -> do
        f0 <- lastFrame dr
        map (frameRow f0) [0 .. 4]
          `shouldBe` [ "┌──────────┐"
                     , "│hi        │"
                     , "│          │"
                     , "│          │"
                     , "└──────────┘"
                     ]
        gridWidth (frameGrid f0) `shouldBe` 12
        gridHeight (frameGrid f0) `shouldBe` 5
        stopApp dr

    it "handles 2-, 1-, 3-, and 0-cell dimensions" $
      withApp defaultVtyAppConfig (10, 4) boxApp $ \dr -> do
        f2 <- resizeApp dr (2, 2)
        frameRow f2 0 `shouldBe` "┌┐"
        frameRow f2 1 `shouldBe` "└┘"
        gridWidth (frameGrid f2) `shouldBe` 2
        f1 <- resizeApp dr (1, 2)
        frameRow f1 0 `shouldBe` " "
        frameRow f1 1 `shouldBe` " "
        f3 <- resizeApp dr (3, 1)
        frameRow f3 0 `shouldBe` " ─ "
        f0 <- resizeApp dr (0, 1)
        gridWidth (frameGrid f0) `shouldBe` 0
        stopApp dr

  describe "boxTitle through the production host" $ do
    it "renders all title alignments at a narrow width" $
      withApp defaultVtyAppConfig (8, 3) titleApp $ \dr -> do
        fCenter <- lastFrame dr
        frameRow fCenter 0 `shouldBe` "┌──Hi──┐"
        fLeft <- stepCommand dr TextAlignment_Left
        frameRow fLeft 0 `shouldBe` "┌Hi────┐"
        fRight <- stepCommand dr TextAlignment_Right
        frameRow fRight 0 `shouldBe` "┌────Hi┐"
        fCenterAgain <- stepCommand dr TextAlignment_Center
        frameRow fCenterAgain 0 `shouldBe` "┌──Hi──┐"
        stopApp dr

    it "clips a title wider than the box interior" $
      withApp defaultVtyAppConfig (8, 3) clipTitleApp $ \dr -> do
        f0 <- lastFrame dr
        frameRow f0 0 `shouldBe` "┌ABCDEF┐"
        stopApp dr

  describe "box through the production host" $ do
    it "translates clicks at screen coordinates to the inner coordinate system" $
      withApp defaultVtyAppConfig (10, 5) translateApp $ \dr -> do
        f0 <- lastFrame dr
        frameObservation f0 `shouldBe` (-1, -1)
        fClick <- stepInput dr (V.EvMouseDown 3 2 V.BLeft [])
        frameObservation fClick `shouldBe` (2, 1)
        fCorner <- stepInput dr (V.EvMouseDown 1 1 V.BLeft [])
        frameObservation fCorner `shouldBe` (0, 0)
        stopApp dr

    it "crops an oversized child image to the box interior" $
      withApp defaultVtyAppConfig (10, 4) clipApp $ \dr -> do
        f0 <- lastFrame dr
        frameRow f0 0 `shouldBe` "┌────────┐"
        frameRow f0 1 `shouldBe` "│XXXXXXXX│"
        frameRow f0 2 `shouldBe` "│        │"
        frameRow f0 3 `shouldBe` "└────────┘"
        gridWidth (frameGrid f0) `shouldBe` 10
        gridHeight (frameGrid f0) `shouldBe` 4
        stopApp dr

  describe "hRule through the production host" $ do
    it "fills its region and handles 2-, 1-, and 0-cell widths" $
      withApp defaultVtyAppConfig (6, 2) hRuleApp $ \dr -> do
        f0 <- lastFrame dr
        frameRow f0 0 `shouldBe` "──────"
        frameRow f0 1 `shouldBe` "──────"
        f2 <- resizeApp dr (2, 1)
        frameRow f2 0 `shouldBe` "──"
        f1 <- resizeApp dr (1, 1)
        frameRow f1 0 `shouldBe` "─"
        f0w <- resizeApp dr (0, 1)
        gridWidth (frameGrid f0w) `shouldBe` 0
        stopApp dr
