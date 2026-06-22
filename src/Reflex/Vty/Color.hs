-- |
-- Module: Reflex.Vty.Color
-- Description: Color manipulation and gradient utilities
module Reflex.Vty.Color
  ( -- * RGB color type
    RGB (..)
  , toRGB
  , fromRGB

    -- * Color operations
  , darken
  , lighten
  , complementary
  , mix
  , alpha

    -- * Gradients
  , Gradient1D (..)
  , gradient1D
  , sampleGradient1D
  , Gradient2D (..)
  , gradient2D
  , sampleGradient2D
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import qualified Graphics.Vty as V

----------------------------------------------------------------------------
-- RGB color type
----------------------------------------------------------------------------

-- | An sRGB color triplet. This is the working type for all color
-- operations; convert to\/from vty's 'V.Color' with 'toRGB' \/ 'fromRGB'.
data RGB = RGB
  { rgbRed :: !Word8
  , rgbGreen :: !Word8
  , rgbBlue :: !Word8
  }
  deriving (Eq, Ord, Show)

-- | Extract 'RGB' components from a vty 'V.Color'. Returns 'Nothing' for
-- 'ISOColor' and 'Color240' (which require a palette lookup).
toRGB :: V.Color -> Maybe RGB
toRGB (V.RGBColor r g b) = Just (RGB r g b)
toRGB _ = Nothing

-- | Convert an 'RGB' triplet to a vty 'V.Color' as an 'RGBColor'.
fromRGB :: RGB -> V.Color
fromRGB (RGB r g b) = V.RGBColor r g b

----------------------------------------------------------------------------
-- Color operations
----------------------------------------------------------------------------

-- | Darken a color by a factor.
-- @darken 0.0@ = black, @darken 1.0@ = unchanged.
darken :: Double -> RGB -> RGB
darken factor (RGB r g b) =
  RGB (scale r) (scale g) (scale b)
  where
    scale c = clampByte (fromIntegral c * clamp01 factor)

-- | Lighten a color by a factor (towards white).
-- @lighten 0.0@ = unchanged, @lighten 1.0@ = white.
lighten :: Double -> RGB -> RGB
lighten factor (RGB r g b) =
  RGB (lift r) (lift g) (lift b)
  where
    f = clamp01 factor
    lift c = clampByte (fromIntegral c + (255 - fromIntegral c) * f)

-- | Complementary color (RGB inversion).
complementary :: RGB -> RGB
complementary (RGB r g b) =
  RGB (255 - r) (255 - g) (255 - b)

-- | Mix two colors.
-- @mix 0.0 a b@ = @a@, @mix 1.0 a b@ = @b@, @mix 0.5 a b@ = average.
mix :: Double -> RGB -> RGB -> RGB
mix t (RGB r1 g1 b1) (RGB r2 g2 b2) =
  RGB (blend r1 r2) (blend g1 g2) (blend b1 b2)
  where
    f = clamp01 t
    blend a b = clampByte (fromIntegral a * (1 - f) + fromIntegral b * f)

-- | Alpha-blend a foreground over a background.
-- @alpha 0.0 fg bg@ = @bg@ (transparent), @alpha 1.0 fg bg@ = @fg@ (opaque).
alpha :: Double -> RGB -> RGB -> RGB
alpha t fg bg = mix t bg fg

----------------------------------------------------------------------------
-- Gradients (1D)
----------------------------------------------------------------------------

-- | A 1D gradient defined by color stops at positions [0, 1].
data Gradient1D = Gradient1D
  { gradient1DStops :: [(Double, RGB)]
  -- ^ Sorted list of (position, color) pairs.
  }

-- | Create a 1D gradient from a list of stops. Stops are sorted by
-- position. Positions should be in [0, 1].
gradient1D :: [(Double, RGB)] -> Gradient1D
gradient1D = Gradient1D . sortBy (comparing fst)

-- | Sample a 1D gradient at the given position [0, 1].
-- Linearly interpolates between the two nearest stops.
sampleGradient1D :: Gradient1D -> Double -> RGB
sampleGradient1D (Gradient1D stops) pos
  | null stops = RGB 0 0 0
  | pos <= 0 = snd (headStops stops)
  | pos >= 1 = snd (last stops)
  | otherwise = go stops
  where
    headStops :: [(Double, RGB)] -> (Double, RGB)
    headStops ((_, c) : _) = (0, c)
    headStops [] = (0, RGB 0 0 0)
    go ((p1, c1) : rest@((p2, c2) : _))
      | pos <= p2 = lerp (pos - p1) (p2 - p1) c1 c2
      | otherwise = go rest
    go [(_, c)] = c
    go [] = RGB 0 0 0

----------------------------------------------------------------------------
-- Gradients (2D)
----------------------------------------------------------------------------

-- | A 2D gradient defined by four corner colors.
data Gradient2D = Gradient2D
  { gradient2DTopLeft :: !RGB
  , gradient2DTopRight :: !RGB
  , gradient2DBottomLeft :: !RGB
  , gradient2DBottomRight :: !RGB
  }

-- | Create a 2D gradient from four corner colors (TL, TR, BL, BR).
gradient2D :: RGB -> RGB -> RGB -> RGB -> Gradient2D
gradient2D = Gradient2D

-- | Sample a 2D gradient at the given (x, y) position, each in [0, 1].
-- Uses bilinear interpolation between the four corners.
sampleGradient2D :: Gradient2D -> Double -> Double -> RGB
sampleGradient2D (Gradient2D tl tr bl br) x y =
  mix y (mix x tl tr) (mix x bl br)

----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------

lerp :: Double -> Double -> RGB -> RGB -> RGB
lerp t range c1 c2
  | range <= 0 = c2
  | otherwise = mix (t / range) c1 c2

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

clampByte :: Double -> Word8
clampByte x = fromIntegral (round (max 0 (min (255 :: Double) x)) :: Int)
