{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.ColorProfileSpec (spec) where

import Control.Monad (forM_)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as V.Color
import Test.Hspec

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
          V.Color.Color240 {} -> True
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

  describe "applyProfile (fields)" $ do
    it "downsamples both foreground and background under Ansi256" $ do
      let attr = V.withBackColor (V.withForeColor V.defAttr (V.Color.RGBColor 10 20 30)) (V.Color.RGBColor 40 50 60)
          applied = applyProfile ColorProfile_Ansi256 attr
      V.attrForeColor applied `shouldSatisfy` \case
        V.SetTo (V.Color.Color240 _) -> True
        _ -> False
      V.attrBackColor applied `shouldSatisfy` \case
        V.SetTo (V.Color.Color240 _) -> True
        _ -> False
    it "resets only the color fields of a fully transparent attr under Ascii" $ do
      let attr = V.Attr V.KeepCurrent V.KeepCurrent V.KeepCurrent V.KeepCurrent
          applied = applyProfile ColorProfile_Ascii attr
      -- Ascii supports no colors at all. Color requests collapse to
      -- Default, even transparent ones. Style and URL survive.
      V.attrForeColor applied `shouldBe` V.Default
      V.attrBackColor applied `shouldBe` V.Default
      V.attrStyle applied `shouldBe` V.KeepCurrent
      V.attrURL applied `shouldBe` V.KeepCurrent
    it "keeps ISO colors under Ansi256" $ do
      let attr = V.withBackColor (V.withForeColor V.defAttr V.Color.blue) V.Color.red
          applied = applyProfile ColorProfile_Ansi256 attr
      V.attrForeColor applied `shouldBe` V.SetTo V.Color.blue
      V.attrBackColor applied `shouldBe` V.SetTo V.Color.red
    it "only ever produces valid output colors" $ do
      let attr = V.withBackColor (V.withForeColor V.defAttr (V.Color.RGBColor 1 2 3)) (V.Color.RGBColor 250 251 252)
      forM_ [ColorProfile_TrueColor, ColorProfile_Ansi256, ColorProfile_Ansi16] $ \profile ->
        forM_ [V.attrForeColor, V.attrBackColor] $ \sel ->
          sel (applyProfile profile attr) `shouldSatisfy` \case
            V.SetTo V.Color.RGBColor {} -> profile == ColorProfile_TrueColor
            V.SetTo (V.Color.Color240 _) -> profile == ColorProfile_Ansi256
            V.SetTo V.Color.ISOColor {} -> profile == ColorProfile_Ansi16
            _ -> False
