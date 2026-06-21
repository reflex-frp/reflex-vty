{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Test.GoldenSpec (spec) where

import Test.Hspec

import Reflex.Vty.Style
import Reflex.Vty.Test.Snapshot

spec :: Spec
spec = describe "Golden snapshots" $ do
  -- Borders
  golden "border_single" $ render (withBorder singleBorder def) "Box"
  golden "border_rounded" $ render (withBorder roundedBorder def) "Box"
  golden "border_thick" $ render (withBorder thickBorder def) "Box"
  golden "border_double" $ render (withBorder doubleBorder def) "Box"
  golden "border_ascii" $ render (withBorder asciiBorder def) "Box"

  -- Padding & Margin
  golden "padding_1" $ render (withPadding 1 1 1 1 def) "pad"
  golden "padding_2" $ render (withPadding 2 2 2 2 def) "pad"
  golden "margin_1" $ render (withMargin 1 1 1 1 def) "m"

  -- Colors
  golden "color_red_fg" $ render (withForeground red def) "Red"
  golden "color_blue_bg" $ render (withBackground blue def) "Blue"
  golden "color_rgb" $ render (withForeground (rgbColor 200 100 50) def) "RGB"

  -- Transforms
  golden "transform_bold" $ render (withBold def) "Bold"
  golden "transform_italic" $ render (withItalic def) "Italic"
  golden "transform_underline" $ render (withUnderline UnderlineSingle def) "UL"
  golden "transform_reverse" $ render (withReverse def) "Rev"

  -- Alignment
  golden "align_left" $ render (withAlignH HAlignLeft . withWidth 20 $ def) "Left"
  golden "align_center" $ render (withAlignH HAlignCenter . withWidth 20 $ def) "Center"
  golden "align_right" $ render (withAlignH HAlignRight . withWidth 20 $ def) "Right"

  -- Combined
  golden "combined" $
    render
      ( withBorder roundedBorder
      . withPadding 1 2 1 2
      . withForeground brightGreen
      . withBorderForeground brightMagenta
      $ def
      )
      "Combined"

  -- Multi-line
  golden "multiline" $ render def "Hello\nWorld"

  -- Wide chars
  golden "wide_chars" $ render def "a文b"

  -- Plain text (baseline)
  golden "plain" $ render def "hello"
  where
    golden name img =
      it name $
        assertGolden name $
          renderGrid $
            imageToGrid img
