{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.CanvasSpec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Canvas

spec :: Spec
spec = describe "Reflex.Vty.Canvas" $ do
  let red = V.withForeColor V.defAttr (V.ISOColor 1)
      blue = V.withForeColor V.defAttr (V.ISOColor 4)
      fromList = Map.fromList :: [((Int, Int), (Char, V.Attr))] -> Map (Int, Int) (Char, V.Attr)

  describe "blankCanvas" $ do
    it "creates a transparent canvas" $ do
      let c = blankCanvas 5 3
      canvasWidth c `shouldBe` 5
      canvasHeight c `shouldBe` 3
      canvasCells c `shouldBe` mempty

    it "cellAt returns Nothing for blank" $ do
      let c = blankCanvas 3 3
      canvasCellAt 1 1 c `shouldBe` Nothing

  describe "place" $ do
    it "overlays opaque cells" $ do
      let dst = blankCanvas 5 3
          src = Canvas 2 1 (fromList [((0, 0), ('X', red))])
          result = place 1 0 src dst
      canvasCellAt 1 0 result `shouldBe` Just ('X', red)

    it "transparent source cells don't overwrite" $ do
      let dst = Canvas 3 1 (fromList [((0, 0), ('A', blue))])
          src = blankCanvas 3 1
          result = place 0 0 src dst
      canvasCellAt 0 0 result `shouldBe` Just ('A', blue)

    it "clips cells outside destination bounds" $ do
      let dst = blankCanvas 3 3
          src = Canvas 2 1 (fromList [((0, 0), ('X', red))])
          result = place 5 5 src dst
      canvasCells result `shouldBe` mempty

  describe "translate" $ do
    it "shifts cells by offset" $ do
      let c = Canvas 5 5 (fromList [((0, 0), ('X', red))])
          result = translate 2 3 c
      canvasCellAt 2 3 result `shouldBe` Just ('X', red)
      canvasCellAt 0 0 result `shouldBe` Nothing

    it "drops cells that move outside bounds" $ do
      let c = Canvas 3 3 (fromList [((0, 0), ('X', red))])
          result = translate 5 0 c
      canvasCells result `shouldBe` mempty

  describe "stack" $ do
    it "later canvases overlay earlier ones" $ do
      let bottom = Canvas 3 1 (fromList [((0, 0), ('A', red))])
          top = Canvas 3 1 (fromList [((0, 0), ('B', blue))])
          result = stack [bottom, top]
      canvasCellAt 0 0 result `shouldBe` Just ('B', blue)

    it "transparent cells in top layer show bottom" $ do
      let bottom = Canvas 3 1 (fromList [((0, 0), ('A', red))])
          top = Canvas 3 1 (fromList [((1, 0), ('B', blue))])
          result = stack [bottom, top]
      canvasCellAt 0 0 result `shouldBe` Just ('A', red)
      canvasCellAt 1 0 result `shouldBe` Just ('B', blue)

    it "empty list returns blank" $ do
      let result = stack []
      canvasWidth result `shouldBe` 0

  describe "imageToCanvas" $ do
    it "converts plain text" $ do
      let img = V.text' red "AB"
          c = imageToCanvas img
      canvasWidth c `shouldBe` 2
      canvasHeight c `shouldBe` 1
      canvasCellAt 0 0 c `shouldBe` Just ('A', red)
      canvasCellAt 1 0 c `shouldBe` Just ('B', red)

    it "converts charFill" $ do
      let img = V.charFill red 'X' 3 2
          c = imageToCanvas img
      canvasWidth c `shouldBe` 3
      canvasHeight c `shouldBe` 2
      canvasCellAt 0 0 c `shouldBe` Just ('X', red)
      canvasCellAt 2 1 c `shouldBe` Just ('X', red)

  describe "canvasToImage" $ do
    it "round-trips through imageToCanvas for opaque content" $ do
      let img = V.text' red "Hello"
          c = imageToCanvas img
          img' = canvasToImage c
      V.imageWidth img' `shouldBe` 5
      V.imageHeight img' `shouldBe` 1

    it "preserves dimensions for transparent canvas" $ do
      let c = blankCanvas 4 2
          img = canvasToImage c
      V.imageWidth img `shouldBe` 4
      V.imageHeight img `shouldBe` 2
