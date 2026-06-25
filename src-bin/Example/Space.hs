-- |
--   Description: A drifting starfield, à la the space example.
module Example.Space (space) where

import Control.Monad.IO.Class (liftIO)
import Data.Bits (shiftR, xor)
import qualified Data.Map as Map
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | A starfield warping leftward. Each star has a pseudo-random row and speed
-- (its "depth"); faster stars are brighter and drawn with a bolder glyph.
space :: Demo t m => m (Event t ())
space = do
  t0 <- liftIO getCurrentTime
  tick <- tickLossy 0.06 t0
  n <- foldDyn (\_ i -> i + 1) (0 :: Int) tick
  dw <- displayWidth
  dh <- displayHeight
  tellImages $ (\w h i -> [starfield w h i]) <$> current dw <*> current dh <*> current n
  quit
  where
    starfield w h i
      | w <= 0 || h <= 0 = V.emptyImage
      | otherwise =
          let nStars = max 1 (w * h `div` 14)
              stars = Map.fromList [star w h i k | k <- [0 .. nStars - 1]]
              mkRow y = V.horizCat [Map.findWithDefault gap (x, y) stars | x <- [0 .. w - 1]]
          in V.vertCat [mkRow y | y <- [0 .. h - 1]]
    gap = V.char V.defAttr ' '
    star w h i k =
      let y = hash (k * 2 + 1) `mod` h
          speed = 1 + hash (k * 3 + 2) `mod` 3
          x = (hash k - i * speed) `mod` w
          (ch, clr) = case speed of
            1 -> ('·', rgbColor 90 90 120)
            2 -> ('*', rgbColor 170 170 205)
            _ -> ('✦', rgbColor 255 255 255)
      in ((x, y), V.char (V.withForeColor V.defAttr clr) ch)
    -- A cheap integer hash, always non-negative.
    hash :: Int -> Int
    hash k0 =
      let a = k0 * 374761393 + 668265263
          b = (a `xor` (a `shiftR` 13)) * 1274126177
          c = b `xor` (b `shiftR` 16)
      in abs c
