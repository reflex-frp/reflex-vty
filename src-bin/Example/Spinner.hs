-- |
--   Description: An animated spinner, à la bubbles\/spinner.
module Example.Spinner (spinner) where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | A braille spinner that cycles a frame every 80ms.
spinner :: Demo t m => m (Event t ())
spinner = do
  t0 <- liftIO getCurrentTime
  tick <- tickLossy 0.08 t0
  idx <- foldDyn (\_ i -> (i + 1) `mod` n) 0 tick
  tellImages $ current $ ffor idx frame
  quit
  where
    frames = "⣾⣽⣻⢿⡿⣟⣯⣷"
    n = length frames
    frame i =
      [ V.horizCat
          [ V.char (V.withForeColor V.defAttr accent) (frames !! i)
          , V.text' (V.withForeColor V.defAttr dim) "  Spinning forever…  (q to quit)"
          ]
      ]
