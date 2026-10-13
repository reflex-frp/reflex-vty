{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Description: Behavior-covering specs for the text rendering widgets,
-- driven through the production host.
module Reflex.Vty.Widget.TextSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex
import Test.Hspec

import Data.Text.Zipper (TextAlignment (..), textWidth)
import Reflex.Vty.Host
import Reflex.Vty.Test.AppHarness
import Reflex.Vty.Test.Snapshot (Grid (..), RenderSpan (..), attrAt, imageText)
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Layout
import Reflex.Vty.Widget.Text

-- | Commands driving the dynamic inputs of the text widgets.
data TextCmd
  = SetText Text
  | SetFill Char
  deriving (Eq, Show)

spec :: Spec
spec = describe "Reflex.Vty.Widget.Text" $ do
  describe "rendered through the production host" $ do
    it "renders text, display, fill, and rich text in their layout rows" $
      withApp defaultVtyAppConfig (20, 7) textApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            frameObservation f `shouldBe` "alpha beta"
            gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "alpha beta"
            gridRow (frameGrid f) 1 `shouldBe` fullRow 20 ""
            gridRow (frameGrid f) 5 `shouldBe` fullRow 20 "10"
            gridRow (frameGrid f) 6 `shouldBe` replicate 20 '#'
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

    it "re-renders every row when the value changes via command" $
      withApp defaultVtyAppConfig (20, 7) textApp $ \driver -> do
        f <- stepCommand driver (SetText "wrap this longer line")
        frameObservation f `shouldBe` "wrap this longer line"
        gridRow (frameGrid f) 0 `shouldBe` fullRow 20 "wrap this longer"
        gridRow (frameGrid f) 1 `shouldBe` fullRow 20 "line"
        gridRow (frameGrid f) 5 `shouldBe` fullRow 20 "21"
        stopApp driver

    it "re-wraps to the new width on resize" $
      withApp defaultVtyAppConfig (20, 7) textApp $ \driver -> do
        _ <- stepCommand driver (SetText "wrap this longer line")
        f <- resizeApp driver (8, 7)
        frameObservation f `shouldBe` "wrap this longer line"
        gridRow (frameGrid f) 0 `shouldBe` fullRow 8 "wrap"
        gridRow (frameGrid f) 1 `shouldBe` fullRow 8 "this"
        gridRow (frameGrid f) 5 `shouldBe` fullRow 8 "21"
        gridRow (frameGrid f) 6 `shouldBe` replicate 8 '#'
        stopApp driver

    it "tracks a dynamic fill character" $
      withApp defaultVtyAppConfig (20, 7) textApp $ \driver -> do
        f <- stepCommand driver (SetFill 'x')
        gridRow (frameGrid f) 6 `shouldBe` replicate 20 'x'
        stopApp driver

    it "applies rich text attributes to the rendered cells" $
      withApp defaultVtyAppConfig (20, 7) textApp $ \driver -> do
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            let mAttr = attrAt 0 4 (frameGrid f)
            mAttr `shouldSatisfy` maybe False (\a -> V.hasStyle (V.fixedStyle a) V.bold)
            attrAt 0 5 (frameGrid f) `shouldSatisfy` maybe False (\a -> V.fixedStyle a == V.defaultStyleMask)
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

    it "renders center and right alignment as unpadded wrapped rows" $
      withApp defaultVtyAppConfig (20, 7) textApp $ \driver -> do
        -- wrapTextImage renders only the wrapped text of each line; the
        -- computed alignment offsets are not turned into padding.
        fs <- driverFrames driver
        case reverse fs of
          (f : _) -> do
            gridRow (frameGrid f) 2 `shouldBe` fullRow 20 "alpha beta"
            gridRow (frameGrid f) 3 `shouldBe` fullRow 20 "alpha beta"
            gridRow (frameGrid f) 2 `shouldBe` gridRow (frameGrid f) 0
          [] -> expectationFailure "expected an initial frame"
        stopApp driver

  describe "wrapTextImage" $ do
    it "wraps words to the given width" $
      imageText (wrapTextImage TextAlignment_Left 8 V.defAttr "aaa bb ccc")
        `shouldBe` ["aaa bb", "ccc   "]

    it "splits on explicit newlines before wrapping" $
      imageText (wrapTextImage TextAlignment_Left 20 V.defAttr "a\nbb cc dd")
        `shouldBe` ["a       ", "bb cc dd"]

    it "accounts for wide characters when measuring width" $ do
      -- "世界" fills the 4-column width exactly; "ab" wraps to the
      -- second row and is padded out to the same width.
      let img = wrapTextImage TextAlignment_Left 4 V.defAttr "世界ab"
      imageText img `shouldBe` ["世界", "ab  "]
      V.imageWidth img `shouldBe` 4
      V.imageHeight img `shouldBe` 2

    it "renders alignment variants as the same wrapped rows" $ do
      let rows alignment = imageText (wrapTextImage alignment 6 V.defAttr "one two three")
      rows TextAlignment_Center `shouldBe` rows TextAlignment_Left
      rows TextAlignment_Right `shouldBe` rows TextAlignment_Left
      rows TextAlignment_Left `shouldBe` ["one  ", "two  ", "three"]

    it "returns no rows for a non-positive width" $
      imageText (wrapTextImage TextAlignment_Left 0 V.defAttr "abc") `shouldBe` []

-- | A stack of text widgets in fixed layout rows:
--
-- * rows 0-1: 'text' (wraps to two rows)
-- * row 2: 'textWithAlignment' centered
-- * row 3: 'textWithAlignment' right-aligned
-- * row 4: 'richText' with a bold attribute
-- * row 5: 'display' of the text length
-- * row 6: 'fill' with a dynamic character
textApp :: TestApp TextCmd Text
textApp cmd = do
  let setTexts = fforMaybe cmd $ \c -> case c of
        SetText t -> Just t
        SetFill _ -> Nothing
      setFills = fforMaybe cmd $ \c -> case c of
        SetFill ch -> Just ch
        SetText _ -> Nothing
  t <- holdDyn "alpha beta" setTexts
  fillChar <- holdDyn '#' setFills
  _ <- initManager_ $
    col $ do
      grout (fixed (pure 2)) $ text (current t)
      grout (fixed (pure 1)) $ textWithAlignment TextAlignment_Center (current t)
      grout (fixed (pure 1)) $ textWithAlignment TextAlignment_Right (current t)
      grout (fixed (pure 1)) $
        richText
          RichTextConfig {_richTextConfig_attributes = pure (V.withStyle V.defAttr V.bold)}
          (current t)
      grout (fixed (pure 1)) $ display (T.length <$> current t)
      grout (fixed (pure 1)) $ fill (current fillChar)
  pure (t, never)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | The text of the @y@-th rendered row of a grid.
gridRow :: Grid -> Int -> String
gridRow g i = case drop i (rowStrings g) of
  (r : _) -> r
  [] -> ""

rowStrings :: Grid -> [String]
rowStrings = map (concatMap (T.unpack . renderSpanText)) . gridRows

-- | Pad a row string with trailing blanks to a full terminal row of the
-- given display width.
fullRow :: Int -> String -> String
fullRow w s = s ++ replicate (w - textWidth (T.pack s)) ' '
