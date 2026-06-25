-- |
--   Description: The classic Doom fire effect, à la the doom-fire example.
module Example.DoomFire (doomFire) where

import Control.Monad.IO.Class (liftIO)
import Data.Bits (shiftR, xor)
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

maxHeat :: Int
maxHeat = 36

-- | A heat grid seeded from a hot bottom row; each frame every cell cools and
-- drifts upward from the cell below it, producing rising flames. The grid is
-- sized to the terminal when the demo starts.
doomFire :: Demo t m => m (Event t ())
doomFire = do
  w <- sample . current =<< displayWidth
  h <- sample . current =<< displayHeight
  t0 <- liftIO getCurrentTime
  tick <- tickLossy 0.05 t0
  st <- foldDyn (\_ (g, f) -> (step w h f g, f + 1)) (initGrid w h, 0 :: Int) tick
  tellImages $ current $ ffor st $ \(g, _) -> [renderFire g]
  quit
  where
    initGrid w h =
      [[if y == h - 1 then maxHeat else 0 | _ <- [0 .. w - 1]] | y <- [0 .. h - 1]]
    step w h f grid =
      let stepRow y below =
            [ let r = rnd x y f
                  srcX = max 0 $ min (w - 1) $ x + ((r `div` 5) `mod` 3 - 1)
                  decay = r `mod` 5
              in max 0 (below !! srcX - decay)
            | x <- [0 .. w - 1]
            ]
      in [stepRow y (grid !! (y + 1)) | y <- [0 .. h - 2]] ++ [replicate w maxHeat]
    renderFire grid = V.vertCat [V.horizCat (map cell rowHeats) | rowHeats <- grid]
    cell heat
      | heat <= 0 = V.char V.defAttr ' '
      | otherwise = V.char (V.withForeColor V.defAttr (heatColor heat)) (heatChar heat)
    heatChar heat
      | heat < 10 = '░'
      | heat < 19 = '▒'
      | heat < 28 = '▓'
      | otherwise = '█'
    heatColor heat = fromRGB $ sampleGradient1D fireGrad (fromIntegral heat / fromIntegral maxHeat)
    fireGrad =
      gradient1D
        [ (0.0, RGB 12 8 8)
        , (0.18, RGB 90 0 0)
        , (0.4, RGB 210 35 0)
        , (0.6, RGB 255 110 0)
        , (0.8, RGB 255 200 40)
        , (1.0, RGB 255 255 200)
        ]
    rnd x y f =
      let a = x * 374761393 + y * 668265263 + f * 2246822519 + 12345
          b = (a `xor` (a `shiftR` 13)) * 1274126177
      in abs (b `xor` (b `shiftR` 16))
