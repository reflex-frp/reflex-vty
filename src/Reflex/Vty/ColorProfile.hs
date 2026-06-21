{-|
Module: Reflex.Vty.ColorProfile
Description: Terminal color-capability detection and downsampling

A 'ColorProfile' describes how many colors the terminal can display. 'detectColorProfile'
reads the capability from a vty 'Graphics.Vty.Output.Output' handle, and 'applyProfile'
downsamples an 'Graphics.Vty.Attr' so that colors and styles the terminal cannot render are
replaced with the closest approximation (or dropped entirely for 'ColorProfile_Ascii' and
'ColorProfile_NoTTY').

This is the reflex-vty analogue of @charmbracelet/colorprofile@. Widgets build with
'Graphics.Vty.Attributes.Color.RGBColor' (true color) and the host downsamples once per frame
at the 'Graphics.Vty.Picture' boundary, so widget code never needs to branch on terminal
capability.
-}
module Reflex.Vty.ColorProfile
  ( ColorProfile(..)
  , colorProfileFromColorMode
  , colorProfileFromVty
  , detectColorProfile
  , convertColor
  , applyProfile
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as V.Color
import qualified Graphics.Vty.Attributes.Color240 as V.Color240
import qualified Graphics.Vty.Output as V.Output

-- | The color rendering capability of a terminal. Ordered from richest to
-- poorest; see 'applyProfile' for how each profile affects an 'V.Attr'.
data ColorProfile
  = ColorProfile_TrueColor
  -- ^ 24-bit RGB; any color renders verbatim.
  | ColorProfile_Ansi256
  -- ^ 8-bit (216-color cube + 24 greys); 'V.Color.RGBColor' is downsampled
  -- to 'V.Color.Color240'.
  | ColorProfile_Ansi16
  -- ^ 4-bit (the 16 ANSI colors); 'V.Color.RGBColor' and 'V.Color.Color240'
  -- are downsampled to the nearest 'V.Color.ISOColor'.
  | ColorProfile_Ascii
  -- ^ No color support; all colors are stripped.
  | ColorProfile_NoTTY
  -- ^ Not a TTY at all; all ANSI escapes (colors /and/ styles) are stripped.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Map vty's 'V.Color.ColorMode' to a 'ColorProfile'. 'V.Color.ColorMode240'
-- maps to 'ColorProfile_Ansi256'; 'V.Color.FullColor' to 'ColorProfile_TrueColor';
-- 'V.Color.ColorMode16' to 'ColorProfile_Ansi16'; 'V.Color.ColorMode8' to
-- 'ColorProfile_Ansi16' as well (the 8 ANSI colors are a subset of the 16);
-- 'V.Color.NoColor' to 'ColorProfile_Ascii'.
colorProfileFromColorMode :: V.Color.ColorMode -> ColorProfile
colorProfileFromColorMode = \case
  V.Color.NoColor        -> ColorProfile_Ascii
  V.Color.ColorMode8     -> ColorProfile_Ansi16
  V.Color.ColorMode16    -> ColorProfile_Ansi16
  V.Color.ColorMode240{} -> ColorProfile_Ansi256
  V.Color.FullColor      -> ColorProfile_TrueColor

-- | Read the color profile from a vty 'V.Output.Output' by inspecting its
-- 'V.Output.outputColorMode'.
colorProfileFromVty :: V.Vty -> ColorProfile
colorProfileFromVty = colorProfileFromColorMode . V.Output.outputColorMode . V.outputIface

-- | 'colorProfileFromVty' in 'MonadIO' for convenience at call sites that
-- are already lifted. Reads from the 'V.Vty' handle's output interface.
detectColorProfile :: MonadIO m => V.Vty -> m ColorProfile
detectColorProfile = pure . colorProfileFromVty

-- | Downsample a single 'V.Color' to the closest representation the profile
-- can render. For 'ColorProfile_Ascii' and 'ColorProfile_NoTTY' the color is
-- returned unchanged — callers should use 'applyProfile' to set the
-- surrounding 'V.Attr' field to 'V.Default' instead, since those profiles
-- have no color representation at all.
convertColor :: ColorProfile -> V.Color.Color -> V.Color.Color
convertColor = \case
  ColorProfile_TrueColor -> id
  ColorProfile_Ansi256 -> \case
    V.Color.RGBColor r g b -> V.Color.Color240 (V.Color240.rgbColorToColor240 r g b)
    c                      -> c
  ColorProfile_Ansi16 -> nearestIso
  ColorProfile_Ascii  -> id
  ColorProfile_NoTTY  -> id

-- | Downsample an entire 'V.Attr' for the profile: colors via 'convertColor',
-- and for 'ColorProfile_Ascii' / 'ColorProfile_NoTTY' the foreground,
-- background, and style are all reset to 'V.Default' so no color or style
-- escapes are emitted. The URL field is preserved (hyperlinks are not color).
applyProfile :: ColorProfile -> V.Attr -> V.Attr
applyProfile profile attr = case profile of
  ColorProfile_NoTTY ->
    attr { V.attrStyle = V.Default, V.attrForeColor = V.Default, V.attrBackColor = V.Default }
  ColorProfile_Ascii ->
    attr { V.attrForeColor = V.Default, V.attrBackColor = V.Default }
  _ ->
    attr
      { V.attrForeColor = downsampleColorField V.attrForeColor
      , V.attrBackColor = downsampleColorField V.attrBackColor
      }
  where
    downsampleColorField sel = case sel attr of
      V.SetTo c -> V.SetTo (convertColor profile c)
      other     -> other

-- | Perceptual nearest-neighbor mapping from any 'V.Color' to the closest of
-- the 16 ANSI 'V.Color.ISOColor' values. Uses the standard xterm 16-color
-- RGB table and squared-Euclidean distance in RGB space.
nearestIso :: V.Color.Color -> V.Color.Color
nearestIso c = case c of
  V.Color.ISOColor{}  -> c
  V.Color.Color240 i  -> case V.Color240.color240CodeToRGB i of
    Nothing    -> c
    Just rgb3  -> nearestIsoRGB rgb3
  V.Color.RGBColor r g b -> nearestIsoRGB (fromIntegral r, fromIntegral g, fromIntegral b)

-- | The 16 ANSI colors paired with their standard xterm RGB coordinates.
-- Indices 0-7 are the normal colors; 8-15 are the bright variants.
isoRgbTable :: [(V.Color.Color, (Int, Int, Int))]
isoRgbTable =
  [ (V.Color.black,         (0,0,0))
  , (V.Color.red,           (205,0,0))
  , (V.Color.green,         (0,205,0))
  , (V.Color.yellow,        (205,205,0))
  , (V.Color.blue,          (0,0,205))
  , (V.Color.magenta,       (205,0,205))
  , (V.Color.cyan,          (0,205,205))
  , (V.Color.white,         (229,229,229))
  , (V.Color.brightBlack,   (127,127,127))
  , (V.Color.brightRed,     (255,0,0))
  , (V.Color.brightGreen,   (0,255,0))
  , (V.Color.brightYellow,  (255,255,0))
  , (V.Color.brightBlue,    (0,0,255))
  , (V.Color.brightMagenta, (255,0,255))
  , (V.Color.brightCyan,    (0,255,255))
  , (V.Color.brightWhite,   (255,255,255))
  ]

-- | Squared Euclidean distance between two RGB triples.
rgbDistanceSq :: (Int, Int, Int) -> (Int, Int, Int) -> Int
rgbDistanceSq (r1,g1,b1) (r2,g2,b2) = dr*dr + dg*dg + db*db
  where
    dr = r1 - r2
    dg = g1 - g2
    db = b1 - b2

-- | Find the nearest ANSI-16 color to the given RGB value.
nearestIsoRGB :: (Int, Int, Int) -> V.Color.Color
nearestIsoRGB target =
  fst $ minimumBy (comparing (rgbDistanceSq target . snd)) isoRgbTable
