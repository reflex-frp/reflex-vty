{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.ColorProfileSpec (spec) where

import Test.Hspec

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as V.Color

import Reflex.Vty.ColorProfile

spec :: Spec
spec = describe "Reflex.Vty.ColorProfile" $ do
  describe "colorProfileFromColorMode" $ do
    it "maps NoColor to Ascii" $
      colorProfileFromColorMode V.Color.NoColor `shouldBe` ColorProfile_Ascii
    it "maps ColorMode8 to Ansi16" $
      colorProfileFromColorMode V.Color.ColorMode8 `shouldBe` ColorProfile_Ansi16
    it "maps ColorMode16 to Ansi16" $
      colorProfileFromColorMode V.Color.ColorMode16 `shouldBe` ColorProfile_Ansi16
    it "maps ColorMode240 to Ansi256" $
      colorProfileFromColorMode (V.Color.ColorMode240 0) `shouldBe` ColorProfile_Ansi256
    it "maps FullColor to TrueColor" $
      colorProfileFromColorMode V.Color.FullColor `shouldBe` ColorProfile_TrueColor

  describe "convertColor" $ do
    it "is identity for TrueColor" $
      convertColor ColorProfile_TrueColor (V.Color.RGBColor 10 20 30)
        `shouldBe` V.Color.RGBColor 10 20 30
    it "downsamples RGB to Color240 for Ansi256" $
      convertColor ColorProfile_Ansi256 (V.Color.RGBColor 10 20 30)
        `shouldSatisfy` \case
          V.Color.Color240{} -> True
          _ -> False
    it "is identity for ISOColor under Ansi16" $
      convertColor ColorProfile_Ansi16 V.Color.red `shouldBe` V.Color.red
    it "maps pure black RGB to the black ISO color under Ansi16" $
      convertColor ColorProfile_Ansi16 (V.Color.RGBColor 0 0 0)
        `shouldBe` V.Color.black
    it "maps pure white RGB to the white ISO color under Ansi16" $
      convertColor ColorProfile_Ansi16 (V.Color.RGBColor 255 255 255)
        `shouldBe` V.Color.brightWhite

  describe "applyProfile" $ do
    let redAttr = V.withForeColor V.defAttr V.Color.red
    it "preserves the foreground color under TrueColor" $ do
      V.attrForeColor (applyProfile ColorProfile_TrueColor redAttr)
        `shouldBe` V.SetTo V.Color.red
    it "resets the foreground to Default under Ascii" $ do
      V.attrForeColor (applyProfile ColorProfile_Ascii redAttr)
        `shouldBe` V.Default
    it "resets foreground, background, and style to Default under NoTTY" $ do
      let attr = V.withStyle (V.withForeColor (V.withBackColor V.defAttr V.Color.blue) V.Color.red) V.bold
          applied = applyProfile ColorProfile_NoTTY attr
      V.attrForeColor applied `shouldBe` V.Default
      V.attrBackColor applied `shouldBe` V.Default
      V.attrStyle applied `shouldBe` V.Default
    it "preserves the URL field under NoTTY" $ do
      let attr = V.withURL V.defAttr "https://example.com"
      V.attrURL (applyProfile ColorProfile_NoTTY attr)
        `shouldBe` V.SetTo "https://example.com"
