{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.ColorSpec (spec) where

import qualified Graphics.Vty as V
import Test.Hspec

import Reflex.Vty.Color

spec :: Spec
spec = describe "Reflex.Vty.Color" $ do
  let black = RGB 0 0 0
      white = RGB 255 255 255
      red = RGB 255 0 0
      blue = RGB 0 0 255

  describe "toRGB / fromRGB" $ do
    it "extracts RGB from RGBColor" $ do
      toRGB (V.RGBColor 10 20 30) `shouldBe` Just (RGB 10 20 30)
    it "returns Nothing for ISOColor" $ do
      toRGB (V.ISOColor 1) `shouldBe` Nothing
    it "returns Nothing for Color240" $ do
      toRGB (V.Color240 100) `shouldBe` Nothing
    it "round-trips RGB through fromRGB . toRGB" $ do
      fromRGB red `shouldBe` V.RGBColor 255 0 0

  describe "darken" $ do
    it "black at 0.0 stays black" $ do
      darken 0.0 white `shouldBe` black
    it "white at 1.0 stays white" $ do
      darken 1.0 white `shouldBe` white
    it "halves all channels" $ do
      darken 0.5 (RGB 200 100 50) `shouldBe` RGB 100 50 25

  describe "lighten" $ do
    it "black at 0.0 stays black" $ do
      lighten 0.0 black `shouldBe` black
    it "black at 1.0 becomes white" $ do
      lighten 1.0 black `shouldBe` white
    it "lightens by half towards white" $ do
      lighten 0.5 (RGB 0 0 0) `shouldBe` RGB 128 128 128

  describe "complementary" $ do
    it "inverts all channels" $ do
      complementary (RGB 100 200 50) `shouldBe` RGB 155 55 205
    it "complement of black is white" $ do
      complementary black `shouldBe` white

  describe "mix" $ do
    it "at 0.0 returns first color" $ do
      mix 0.0 red blue `shouldBe` red
    it "at 1.0 returns second color" $ do
      mix 1.0 red blue `shouldBe` blue
    it "at 0.5 averages channels" $ do
      mix 0.5 red blue `shouldBe` RGB 128 0 128

  describe "alpha" $ do
    it "at 0.0 returns background" $ do
      alpha 0.0 red blue `shouldBe` blue
    it "at 1.0 returns foreground" $ do
      alpha 1.0 red blue `shouldBe` red

  describe "Gradient1D" $ do
    let grad = gradient1D [(0.0, black), (1.0, white)]
    it "samples first stop at 0.0" $ do
      sampleGradient1D grad 0.0 `shouldBe` black
    it "samples last stop at 1.0" $ do
      sampleGradient1D grad 1.0 `shouldBe` white
    it "interpolates at 0.5" $ do
      sampleGradient1D grad 0.5 `shouldBe` RGB 128 128 128
    it "clamps below 0.0 to first stop" $ do
      sampleGradient1D grad (-0.5) `shouldBe` black
    it "clamps above 1.0 to last stop" $ do
      sampleGradient1D grad 1.5 `shouldBe` white

    it "handles three stops with correct interpolation" $ do
      let grad3 = gradient1D [(0.0, red), (0.5, white), (1.0, blue)]
      sampleGradient1D grad3 0.25 `shouldBe` RGB 255 128 128
      sampleGradient1D grad3 0.75 `shouldBe` RGB 128 128 255

    it "returns black for empty gradient" $ do
      sampleGradient1D (gradient1D []) 0.5 `shouldBe` black

  describe "Gradient2D" $ do
    let g2 = gradient2D black white red blue
    it "samples corners exactly" $ do
      sampleGradient2D g2 0 0 `shouldBe` black
      sampleGradient2D g2 1 0 `shouldBe` white
      sampleGradient2D g2 0 1 `shouldBe` red
      sampleGradient2D g2 1 1 `shouldBe` blue
    it "interpolates center" $ do
      sampleGradient2D g2 0.5 0.5 `shouldBe` RGB 128 64 128
