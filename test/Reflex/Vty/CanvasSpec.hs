{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.CanvasSpec (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Graphics.Text.Width (wcwidth)
import qualified Graphics.Vty as V
import Test.Hspec
import Test.QuickCheck (Gen, counterexample, forAll, suchThat)

import Reflex.Vty.Canvas
import Reflex.Vty.Style (HAlign (..), placeHorizontal)
import Reflex.Vty.Test.Generators (genDim, genLineText, runPlanProperty)
import Reflex.Vty.Test.Snapshot

red, blue, green :: V.Attr
red = V.withForeColor V.defAttr (V.ISOColor 1)
blue = V.withForeColor V.defAttr (V.ISOColor 4)
green = V.withForeColor V.defAttr (V.ISOColor 2)

cell :: Char -> CanvasCell
cell ch = CanvasCell 1 ((V.defAttr, T.singleton ch) :| [])

wideCell :: Char -> CanvasCell
wideCell ch = CanvasCell 2 ((V.defAttr, T.singleton ch) :| [])

-- | Render a canvas to row text through the independent oracle.
canvasText :: Canvas -> [String]
canvasText = imageText . canvasToImage

-- | Row text the oracle produces for a direct Vty picture. The
-- picture is built from layers; the first layer is topmost.
refLayers :: [V.Image] -> Int -> Int -> [String]
refLayers layers w h =
  map (concatMap (T.unpack . renderSpanText)) (gridRows (pictureToGrid (w, h) (V.picForLayers layers)))

spec :: Spec
spec = describe "Reflex.Vty.Canvas" $ do
  describe "blankCanvas" $ do
    it "is fully transparent" $ do
      let c = blankCanvas 3 3
      canvasCells c `shouldSatisfy` null
      canvasCellAt 1 1 c `shouldBe` Nothing

    it "converts to background fill that lets a lower image show through" $ do
      let top = canvasToImage (blankCanvas 2 1)
          pic = V.picForLayers [top, V.string blue "xy"]
      map (concatMap (T.unpack . renderSpanText)) (gridRows (pictureToGrid (2, 1) pic))
        `shouldBe` ["xy"]
      attrAt 0 0 (pictureToGrid (2, 1) pic) `shouldBe` Just (V.FixedAttr V.defaultStyleMask (Just (V.ISOColor 4)) Nothing Nothing)

  describe "canvasCellAt" $ do
    it "returns the covering glyph at a wide glyph's continuation column" $ do
      let c = imageToCanvas (V.string V.defAttr "a\30028b")
      canvasCellAt 1 0 c `shouldBe` Just (wideCell '\30028')
      canvasCellAt 2 0 c `shouldBe` Just (wideCell '\30028')
      canvasCellAt 0 0 c `shouldBe` Just (cell 'a')
      canvasCellAt 3 0 c `shouldBe` Just (cell 'b')

    it "returns Nothing out of bounds or on transparent cells" $ do
      let c = placeCanvas 0 0 (imageToCanvas (V.string red "b")) (blankCanvas 3 1)
      canvasCellAt (-1) 0 c `shouldBe` Nothing
      canvasCellAt 3 0 c `shouldBe` Nothing
      canvasCellAt 0 1 c `shouldBe` Nothing
      canvasCellAt 1 0 c `shouldBe` Nothing

  describe "imageToCanvas / canvasToImage roundtrips" $ do
    it "preserves ASCII text and attributes after conversion to a canvas" $ do
      let c = imageToCanvas (V.string red "ab")
      canvasCellAt 0 0 c `shouldBe` Just (CanvasCell 1 ((red, "a") :| []))
      canvasCellAt 1 0 c `shouldBe` Just (CanvasCell 1 ((red, "b") :| []))
      canvasText c `shouldBe` ["ab"]

    it "preserves mixed-width text without adding continuation cells" $ do
      let c = imageToCanvas (V.string V.defAttr "a\30028b")
      canvasCells c `shouldSatisfy` not . null
      canvasText c `shouldBe` ["a\30028b"]
      V.imageWidth (canvasToImage c) `shouldBe` 4

    it "preserves a combining mark inside its glyph" $ do
      let c = imageToCanvas (V.string V.defAttr "e\769")
      canvasCellAt 0 0 c `shouldBe` Just (CanvasCell 1 ((V.defAttr, "e\769") :| []))

    it "keeps different attrs across a combining sequence" $ do
      let img = V.horizCat [V.text' red "e", V.text' blue "\769", V.text' green "x"]
          c = imageToCanvas img
      canvasCellAt 0 0 c `shouldBe` Just (CanvasCell 1 ((red, "e") :| [(blue, "\769")]))
      canvasCellAt 1 0 c `shouldBe` Just (CanvasCell 1 ((green, "x") :| []))
      canvasText c `shouldBe` imageText img

    it "converts backgrounds opaquely, including trailing blanks" $ do
      let c = imageToCanvas (V.horizCat [V.string V.defAttr "ab", V.backgroundFill 3 1])
      canvasCellAt 2 0 c `shouldBe` Just (cell ' ')
      canvasCellAt 4 0 c `shouldBe` Just (cell ' ')
      canvasText c `shouldBe` ["ab   "]

    it "has no cells for a zero-area image" $ do
      imageToCanvas V.emptyImage `shouldBe` Canvas 0 0 (canvasCells (blankCanvas 0 0))
      V.imageWidth (canvasToImage (blankCanvas 0 5)) `shouldBe` 0
      V.imageHeight (canvasToImage (blankCanvas 5 0)) `shouldBe` 0

  describe "placeCanvas" $ do
    it "overlays opaque glyphs and leaves transparent holes" $ do
      let src = imageToCanvas (V.string red "b")
          dst = blankCanvas 3 1
          c = placeCanvas 0 0 src dst
      canvasCellAt 0 0 c `shouldBe` Just (CanvasCell 1 ((red, "b") :| []))
      canvasCellAt 2 0 c `shouldBe` Nothing
      -- The holes must let a lower image show through, like Vty layers.
      canvasText c `shouldBe` ["b  "]
      let layered = V.picForLayers [canvasToImage c, V.string blue "xyz"]
      map (concatMap (T.unpack . renderSpanText)) (gridRows (pictureToGrid (3, 1) layered))
        `shouldBe` ["byz"]

    it "clips source glyphs moving past the right edge, removing the combining suffix" $ do
      let src = imageToCanvas (V.string V.defAttr "\30028\769")
          c = placeCanvas 1 0 src (blankCanvas 2 1)
      canvasCellAt 1 0 c `shouldBe` Just (CanvasCell 1 ((V.defAttr, "\8230") :| []))
      canvasText c `shouldBe` [" \8230"]

    it "clips source glyphs entering from the left, retaining the combining suffix" $ do
      let src = imageToCanvas (V.string V.defAttr "\30028\769")
          c = placeCanvas (-1) 0 src (blankCanvas 2 1)
      canvasCellAt 0 0 c `shouldBe` Just (CanvasCell 1 ((V.defAttr, "\8230\769") :| []))
      canvasText c `shouldBe` ["\8230\769 "]

    it "drops width-one glyphs wholly outside" $ do
      let c = translate (-1) 0 (imageToCanvas (V.string V.defAttr "ab"))
      canvasText c `shouldBe` ["b "]

    it "overwriting the second column of a wide glyph matches Vty's clipping" $ do
      let dst = imageToCanvas (V.string V.defAttr "a\30028b")
          c = placeCanvas 2 0 (imageToCanvas (V.string V.defAttr "X")) dst
      canvasText c `shouldBe` refLayers [V.translate 2 (0 :: Int) (V.string V.defAttr "X"), V.string V.defAttr "a\30028b"] 4 1

    it "overwriting the first column of a wide glyph matches Vty's clipping" $ do
      let dst = imageToCanvas (V.string V.defAttr "a\30028b")
          c = placeCanvas 1 0 (imageToCanvas (V.string V.defAttr "X")) dst
      canvasText c `shouldBe` refLayers [V.translate 1 (0 :: Int) (V.string V.defAttr "X"), V.string V.defAttr "a\30028b"] 4 1

    it "removes the combining suffix from the retained half of an overwritten wide glyph" $ do
      let dst = imageToCanvas (V.string V.defAttr "a\30028\769b")
          c = placeCanvas 2 0 (imageToCanvas (V.string V.defAttr "X")) dst
      canvasText c
        `shouldBe` refLayers [V.translate 2 (0 :: Int) (V.string V.defAttr "X"), V.string V.defAttr "a\30028\769b"] 4 1

    it "moves wholly outside the bounds to nothing" $ do
      let c = translate 10 10 (imageToCanvas (V.string V.defAttr "ab"))
      canvasCells c `shouldSatisfy` null
      canvasText c `shouldBe` ["  "]

    it "composes three overlapping layers like Vty" $ do
      let bottom = imageToCanvas (V.string blue "abcd")
          mid = placeCanvas 1 0 (imageToCanvas (V.string green "XY")) bottom
          top = placeCanvas 3 0 (imageToCanvas (V.string V.defAttr "Z")) mid
      canvasText top
        `shouldBe` refLayers
          [ V.translate 3 (0 :: Int) (V.string V.defAttr "Z")
          , V.translate 1 (0 :: Int) (V.string green "XY")
          , V.string blue "abcd"
          ]
          4
          1

    it "onto a zero-size destination yields an empty canvas" $ do
      placeCanvas 0 0 (imageToCanvas (V.string V.defAttr "a")) (blankCanvas 0 0)
        `shouldBe` Canvas 0 0 (canvasCells (blankCanvas 0 0))

  describe "translate" $ do
    it "preserves dimensions" $ do
      let c = translate 1 1 (imageToCanvas (V.string V.defAttr "ab"))
      canvasWidth c `shouldBe` 2
      canvasHeight c `shouldBe` 1
      canvasCellAt 1 1 c `shouldBe` Nothing -- moved off the single row
    it "shifts glyphs within bounds" $ do
      let c = translate 1 0 (imageToCanvas (V.string V.defAttr "ab"))
      canvasCellAt 1 0 c `shouldBe` Just (cell 'a')
      canvasCellAt 2 0 c `shouldBe` Nothing -- 'b' moved past the 2-wide bounds
      canvasCellAt 0 0 c `shouldBe` Nothing

    it "clips a wide glyph at the right edge" $ do
      let c = translate 1 0 (imageToCanvas (V.string V.defAttr "a\30028"))
      canvasText c `shouldBe` [" a\8230"]

    it "clips a wide glyph at the left edge" $ do
      let c = translate (-1) 0 (imageToCanvas (V.string V.defAttr "a\30028"))
      canvasText c `shouldBe` ["\30028 "]

    it "drops content shifted past the bottom" $ do
      let c = translate 0 1 (imageToCanvas (V.string V.defAttr "a"))
      canvasCells c `shouldSatisfy` null

  describe "stack" $ do
    it "stacks nothing into an empty canvas" $ do
      stack [] `shouldBe` blankCanvas 0 0

    it "uses the first canvas's dimensions" $ do
      let c = stack [blankCanvas 2 1, imageToCanvas (V.string V.defAttr "abc")]
      canvasWidth c `shouldBe` 2
      canvasText c `shouldBe` ["ab"]

  describe "layer/translate/clip properties against direct Vty pictures" $ do
    it "opaque roundtrips preserve text, widths, and attributes" $
      runPlanProperty $
        forAll ((,) <$> genDim <*> positiveWidthText) $ \(w, txt) ->
          let img = V.string red (T.unpack txt)
              padded = placeHorizontal HAlignLeft w img
              c = imageToCanvas padded
              rendered = canvasToImage c
              gridC = imageToGrid rendered
              gridRef = imageToGrid padded
          in counterexample (show txt ++ " at width " ++ show w) $
               gridEquals gridC gridRef == Nothing

    it "translations match the identically cropped Vty picture" $
      runPlanProperty $
        forAll ((,,) <$> genDim <*> genDim <*> positiveWidthText) $ \(dx, dy, txt) ->
          let img = V.string red (T.unpack txt)
              w = V.imageWidth img
              h = V.imageHeight img
              c = translate dx dy (imageToCanvas img)
              gridC = imageToGrid (canvasToImage c)
              gridRef = pictureToGrid (w, h) (V.picForImage (V.translate dx dy img))
          in counterexample ("translate " ++ show (dx, dy) ++ " of " ++ show txt) $
               gridEquals gridC gridRef == Nothing

-- | Non-empty text carrying at least one positive-width base glyph.
positiveWidthText :: Gen T.Text
positiveWidthText =
  suchThat genLineText (\t -> not (T.null t) && T.any (\ch -> wcwidth ch > 0) t)
