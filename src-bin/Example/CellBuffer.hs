-- |
--   Description: An animated plasma field rendered cell-by-cell, à la the
--   cellbuffer example.
module Example.CellBuffer (cellBuffer) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | A classic plasma effect: each cell's character and color are a pure
-- function of its position and the frame, summed over a few sine waves.
cellBuffer :: Demo t m => m (Event t ())
cellBuffer = do
  t0 <- liftIO getCurrentTime
  tick <- tickLossy 0.06 t0
  n <- foldDyn (\_ i -> i + 1) (0 :: Int) tick
  dw <- displayWidth
  dh <- displayHeight
  tellImages $ (\w h i -> [plasma w h i]) <$> current dw <*> current dh <*> current n
  quit
  where
    ramp = " .:-=+*#%@"
    grad =
      gradient1D
        [ (0, RGB 25 20 70)
        , (0.4, RGB 190 40 140)
        , (0.7, RGB 255 150 40)
        , (1, RGB 255 240 150)
        ]
    plasma w h i =
      let t = fromIntegral i * 0.12 :: Double
          cell x y =
            let fx = fromIntegral x
                fy = fromIntegral y
                v =
                  sin (fx * 0.18 + t)
                    + sin (fy * 0.27 + t * 1.2)
                    + sin ((fx + fy) * 0.12 + t * 0.8)
                    + sin (sqrt (fx * fx + fy * fy) * 0.2 - t)
                u = (v + 4) / 8 -- normalize roughly to [0,1]
                ci = max 0 $ min (length ramp - 1) $ floor (u * fromIntegral (length ramp))
                clr = fromRGB $ sampleGradient1D grad u
            in V.char (V.withForeColor V.defAttr clr) (ramp !! ci)
      in V.vertCat [V.horizCat [cell x y | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
