{-# LANGUAGE OverloadedStrings #-}

module Reflex.Vty.Test.GoldenSpec (spec) where

import System.FilePath ((</>))
import Test.Hspec

import Reflex.Vty.Style
import Reflex.Vty.Test.Snapshot

goldenDir :: FilePath
goldenDir = "test" </> "goldens"

spec :: GoldenMode -> Spec
spec = describe "Golden snapshots" . withMode
  where
    withMode mode = do
      -- Borders
      golden mode "border_single" $ render (withBorder singleBorder def) "Box"
      golden mode "border_rounded" $ render (withBorder roundedBorder def) "Box"
      golden mode "border_thick" $ render (withBorder thickBorder def) "Box"
      golden mode "border_double" $ render (withBorder doubleBorder def) "Box"
      golden mode "border_ascii" $ render (withBorder asciiBorder def) "Box"

      -- Padding & Margin
      golden mode "padding_1" $ render (withPadding 1 1 1 1 def) "pad"
      golden mode "padding_2" $ render (withPadding 2 2 2 2 def) "pad"
      golden mode "margin_1" $ render (withMargin 1 1 1 1 def) "m"

      -- Colors
      golden mode "color_red_fg" $ render (withForeground red def) "Red"
      golden mode "color_blue_bg" $ render (withBackground blue def) "Blue"
      golden mode "color_rgb" $ render (withForeground (rgbColor 200 100 50) def) "RGB"

      -- Transforms
      golden mode "transform_bold" $ render (withBold def) "Bold"
      golden mode "transform_italic" $ render (withItalic def) "Italic"
      golden mode "transform_underline" $ render (withUnderline UnderlineSingle def) "UL"
      golden mode "transform_reverse" $ render (withReverse def) "Rev"

      -- Alignment
      golden mode "align_left" $ render (withAlignH HAlignLeft . withWidth 20 $ def) "Left"
      golden mode "align_center" $ render (withAlignH HAlignCenter . withWidth 20 $ def) "Center"
      golden mode "align_right" $ render (withAlignH HAlignRight . withWidth 20 $ def) "Right"

      -- Combined
      golden mode "combined" $
        render
          ( withBorder roundedBorder
              . withPadding 1 2 1 2
              . withForeground brightGreen
              . withBorderForeground brightMagenta
              $ def
          )
          "Combined"

      -- Multi-line
      golden mode "multiline" $ render def "Hello\nWorld"

      -- Wide chars
      golden mode "wide_chars" $ render def "a文b"

      -- Plain text (baseline)
      golden mode "plain" $ render def "hello"
    golden mode name img =
      it name $
        assertGolden mode goldenDir name $
          renderGrid $
            imageToGrid img
