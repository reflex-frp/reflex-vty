{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Test.SnapshotSpec (spec) where

import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Test.Snapshot

red, blue :: V.Attr
red = V.withForeColor V.defAttr (V.ISOColor 1)
blue = V.withForeColor V.defAttr (V.ISOColor 4)

redFixed, blueFixed :: V.FixedAttr
redFixed = V.FixedAttr V.defaultStyleMask (Just (V.ISOColor 1)) Nothing Nothing
blueFixed = V.FixedAttr V.defaultStyleMask (Just (V.ISOColor 4)) Nothing Nothing

defaultFixed :: V.FixedAttr
defaultFixed = V.FixedAttr V.defaultStyleMask Nothing Nothing Nothing

spec :: Spec
spec = describe "Reflex.Vty.Test.Snapshot" $ do
  describe "imageToGrid" $ do
    it "empty image has zero dimensions and no rows" $ do
      let g = imageToGrid V.emptyImage
      g `shouldBe` Grid 0 0 []
      imageText V.emptyImage `shouldBe` []

    it "renders plain single-row text" $ do
      let g = imageToGrid (V.string V.defAttr "hello")
      gridWidth g `shouldBe` 5
      gridHeight g `shouldBe` 1
      imageText (V.string V.defAttr "hello") `shouldBe` ["hello"]

    it "wide text occupies its full column width but no continuation space" $ do
      let img = V.string V.defAttr "a\30028b"
      V.imageWidth img `shouldBe` 4
      let g = imageToGrid img
      gridWidth g `shouldBe` 4
      imageText img `shouldBe` ["a\30028b"]
      case gridRows g of
        [[s]] -> do
          renderSpanColumns s `shouldBe` 4
          renderSpanText s `shouldBe` T.pack "a\30028b"
        _ -> expectationFailure "expected exactly one row with one span"

    it "preserves a zero-width combining mark after its base" $ do
      let img = V.string V.defAttr "e\769"
      imageText img `shouldBe` ["e\769"]
      case gridRows (imageToGrid img) of
        [[s]] -> renderSpanColumns s `shouldBe` 1
        _ -> expectationFailure "expected exactly one row with one span"

    it "keeps a trailing combining mark inside its own span at a span boundary" $ do
      let img = V.horizCat [V.text' red "e\769", V.text' blue "x"]
      imageText img `shouldBe` ["e\769x"]
      case gridRows (imageToGrid img) of
        [[s0, s1]] -> do
          renderSpanText s0 `shouldBe` T.pack "e\769"
          renderSpanAttr s0 `shouldBe` redFixed
          renderSpanText s1 `shouldBe` "x"
          renderSpanAttr s1 `shouldBe` blueFixed
        _ -> expectationFailure "expected exactly one row with two spans"

    it "keeps differently styled adjacent spans apart" $ do
      let img = V.horizCat [V.string red "ab", V.string blue "cd"]
      case gridRows (imageToGrid img) of
        [[s0, s1]] -> do
          renderSpanAttr s0 `shouldBe` redFixed
          renderSpanAttr s1 `shouldBe` blueFixed
        _ -> expectationFailure "expected exactly one row with two spans"

    it "resolves URL attributes" $ do
      let img = V.string (V.withURL V.defAttr "http://x") "u"
          g = imageToGrid img
      case gridRows g of
        [(s : _)] -> do
          V.fixedURL (renderSpanAttr s) `shouldBe` Just "http://x"
          V.fixedForeColor (renderSpanAttr s) `shouldBe` Nothing
        _ -> expectationFailure "expected one span"

    it "includes actual trailing blank columns in imageText" $ do
      let img = V.horizCat [V.string V.defAttr "ab", V.backgroundFill 2 1]
      V.imageWidth img `shouldBe` 4
      imageText img `shouldBe` ["ab  "]

    it "coalesces adjacent spans with equal effective attributes" $ do
      let img = V.horizCat [V.text' red "ab", V.text' red "cd"]
      imageText img `shouldBe` ["abcd"]
      case gridRows (imageToGrid img) of
        [[s]] -> do
          renderSpanColumns s `shouldBe` 4
          renderSpanText s `shouldBe` "abcd"
        _ -> expectationFailure "expected exactly one row with one span"

  describe "wide-glyph clipping follows Vty's ellipsis policy" $ do
    it "keeping the left half of a wide glyph renders an ellipsis" $ do
      let img = V.cropRight 2 (V.string V.defAttr "a\30028")
      imageText img `shouldBe` ["a\8230"]
      gridWidth (imageToGrid img) `shouldBe` 2

    it "keeping the right half of a wide glyph renders an ellipsis" $ do
      let img = V.cropLeft 1 (V.string V.defAttr "a\30028")
      imageText img `shouldBe` ["\8230"]
      gridWidth (imageToGrid img) `shouldBe` 1

  describe "joins" $ do
    it "vertical join stacks rows" $ do
      let img = V.vertCat [V.string V.defAttr "ab", V.string red "xy"]
      imageText img `shouldBe` ["ab", "xy"]
      case gridRows (imageToGrid img) of
        [[s0], [s1]] -> do
          renderSpanAttr s0 `shouldBe` defaultFixed
          renderSpanAttr s1 `shouldBe` redFixed
        _ -> expectationFailure "expected two rows with one span each"

    it "horizontal join concatenates spans" $ do
      let img = V.horizCat [V.string red "ab", V.string blue "cd"]
      imageText img `shouldBe` ["abcd"]
      attrAt 0 0 (imageToGrid img) `shouldBe` Just redFixed
      attrAt 2 0 (imageToGrid img) `shouldBe` Just blueFixed

  describe "pictureToGrid" $ do
    it "translates blank backgrounds and content correctly" $ do
      let pic = V.picForImage (V.translate 2 1 (V.string red "x"))
          g = pictureToGrid (5, 3) pic
      gridWidth g `shouldBe` 5
      gridHeight g `shouldBe` 3
      map (concatMap (T.unpack . renderSpanText)) (gridRows g)
        `shouldBe` ["     ", "  x  ", "     "]
      attrAt 2 1 g `shouldBe` Just redFixed
      attrAt 0 1 g `shouldBe` Just defaultFixed

    it "overlapping layers: the first layer is topmost" $ do
      let pic = V.picForLayers [V.translate 2 0 (V.string red "AB"), V.string blue "CD"]
          g = pictureToGrid (4, 1) pic
      concatMap (T.unpack . renderSpanText) (concat (gridRows g)) `shouldBe` "CDAB"
      attrAt 0 0 g `shouldBe` Just blueFixed
      attrAt 2 0 g `shouldBe` Just redFixed

    it "pads short pictures with blank rows up to the requested height" $ do
      let g = pictureToGrid (4, 3) (V.picForImage (V.string V.defAttr "ab"))
      gridHeight g `shouldBe` 3
      [sum (map renderSpanColumns r) | r <- gridRows g] `shouldBe` [4, 4, 4]
      concatMap (T.unpack . renderSpanText) (concat (gridRows g)) `shouldBe` "ab          "

    it "normalizes an empty layer list instead of failing" $ do
      let pic = V.Picture {V.picLayers = [], V.picBackground = V.ClearBackground, V.picCursor = V.NoCursor}
          g = pictureToGrid (4, 2) pic
      gridWidth g `shouldBe` 4
      gridHeight g `shouldBe` 2
      [sum (map renderSpanColumns r) | r <- gridRows g] `shouldBe` [4, 4]

    it "clamps negative requested dimensions to zero" $ do
      let pic = V.picForImage (V.string V.defAttr "ab")
      pictureToGrid (-3, -2) pic `shouldBe` Grid 0 0 []

    it "zero-area region yields the requested dimensions and no rows" $ do
      let pic = V.picForImage (V.string V.defAttr "ab")
      pictureToGrid (0, 5) pic `shouldBe` Grid 0 5 []

  describe "attrAt" $ do
    it "reports the same attribute for both columns of a wide glyph" $ do
      let g = imageToGrid (V.horizCat [V.string red "a\30028", V.string blue "b"])
      attrAt 0 0 g `shouldBe` Just redFixed
      attrAt 1 0 g `shouldBe` Just redFixed
      attrAt 2 0 g `shouldBe` Just redFixed
      attrAt 3 0 g `shouldBe` Just blueFixed

    it "returns Nothing out of bounds" $ do
      let g = imageToGrid (V.string red "ab")
      attrAt 2 0 g `shouldBe` Nothing
      attrAt 0 1 g `shouldBe` Nothing
      attrAt (-1) 0 g `shouldBe` Nothing
      attrAt 0 (-1) g `shouldBe` Nothing

  describe "gridEquals" $ do
    it "returns Nothing for identical grids" $ do
      let g = imageToGrid (V.string red "hi")
      gridEquals g g `shouldBe` Nothing

    it "detects differing text" $ do
      gridEquals
        (imageToGrid (V.string V.defAttr "hi"))
        (imageToGrid (V.string V.defAttr "ho"))
        `shouldSatisfy` isJust

    it "detects differing widths" $ do
      gridEquals
        (imageToGrid (V.string V.defAttr "hi"))
        (imageToGrid (V.horizCat [V.string V.defAttr "hi", V.backgroundFill 1 1]))
        `shouldSatisfy` isJust

    it "detects differing color" $ do
      gridEquals
        (imageToGrid (V.string red "hi"))
        (imageToGrid (V.string blue "hi"))
        `shouldSatisfy` isJust

    it "detects differing URL" $ do
      gridEquals
        (imageToGrid (V.string (V.withURL V.defAttr "http://a") "hi"))
        (imageToGrid (V.string (V.withURL V.defAttr "http://b") "hi"))
        `shouldSatisfy` isJust

  describe "renderGrid" $ do
    it "serializes dimensions, escaped rows, and canonical spans" $ do
      let txt = renderGrid (imageToGrid (V.string red "a\30028b"))
      lines txt
        `shouldBe` [ "4x1"
                   , show "a\30028b"
                   , "--- spans ---"
                   , "0:0+4 text=" ++ show "a\30028b" ++ " attr=" ++ show redFixed
                   ]

    it "serializes default attributes too" $ do
      let txt = renderGrid (imageToGrid (V.string V.defAttr "x"))
      lines txt
        `shouldBe` [ "1x1"
                   , show "x"
                   , "--- spans ---"
                   , "0:0+1 text=" ++ show "x" ++ " attr=" ++ show defaultFixed
                   ]
