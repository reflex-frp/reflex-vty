{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.StyleJoinSpec (spec) where

import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Style

spec :: Spec
spec = describe "Image composition" $ do
  let imgA = V.text' V.defAttr "AB"
      imgB = V.text' V.defAttr "XYZ"

  describe "joinHorizontal" $ do
    it "concatenates images side by side" $ do
      let result = joinHorizontal VAlignTop [imgA, imgB]
      V.imageWidth result `shouldBe` 5

    it "returns empty for empty list" $ do
      V.imageWidth (joinHorizontal VAlignTop []) `shouldBe` 0

    it "pads shorter images to match tallest" $ do
      let tall = V.charFill V.defAttr 'X' 2 3
          short = V.text' V.defAttr "AB"
          result = joinHorizontal VAlignTop [short, tall]
      V.imageHeight result `shouldBe` 3

    it "top-aligns (shorter gets bottom padding)" $ do
      let tall = V.charFill V.defAttr 'X' 2 3
          short = V.text' V.defAttr "AB"
          result = joinHorizontal VAlignTop [short, tall]
      V.imageHeight result `shouldBe` 3

    it "bottom-aligns (shorter gets top padding)" $ do
      let tall = V.charFill V.defAttr 'X' 2 3
          short = V.text' V.defAttr "AB"
          result = joinHorizontal VAlignBottom [short, tall]
      V.imageHeight result `shouldBe` 3

  describe "joinVertical" $ do
    it "stacks images vertically" $ do
      let result = joinVertical HAlignLeft [imgA, imgB]
      V.imageHeight result `shouldBe` 2

    it "returns empty for empty list" $ do
      V.imageHeight (joinVertical HAlignLeft []) `shouldBe` 0

    it "pads narrower images to match widest" $ do
      let wide = V.charFill V.defAttr 'X' 5 1
          narrow = V.text' V.defAttr "AB"
          result = joinVertical HAlignLeft [narrow, wide]
      V.imageWidth result `shouldBe` 5

  describe "placeHorizontal" $ do
    it "left-aligns within target width" $ do
      let result = placeHorizontal HAlignLeft 5 imgA
      V.imageWidth result `shouldBe` 5

    it "right-aligns within target width" $ do
      let result = placeHorizontal HAlignRight 5 imgA
      V.imageWidth result `shouldBe` 5

    it "center-aligns within target width" $ do
      let result = placeHorizontal HAlignCenter 6 imgA
      V.imageWidth result `shouldBe` 6

    it "returns image as-is when already at target width" $ do
      let result = placeHorizontal HAlignLeft 2 imgA
      V.imageWidth result `shouldBe` 2

  describe "placeVertical" $ do
    it "top-aligns within target height" $ do
      let result = placeVertical VAlignTop 3 imgA
      V.imageHeight result `shouldBe` 3

    it "bottom-aligns within target height" $ do
      let result = placeVertical VAlignBottom 3 imgA
      V.imageHeight result `shouldBe` 3

  describe "place" $ do
    it "places within both dimensions" $ do
      let result = place HAlignCenter VAlignMiddle 6 3 imgA
      V.imageWidth result `shouldBe` 6
      V.imageHeight result `shouldBe` 3
