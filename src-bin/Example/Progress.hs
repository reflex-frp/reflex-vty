-- |
--   Description: An animated, gradient-filled progress bar, à la bubbles\/progress.
module Example.Progress (progressBar) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex
import Text.Printf (printf)

import Example.Common
import Reflex.Vty

-- | A progress bar that fills 0→100% (then loops), tinted with a red→yellow→
-- green 'Gradient1D' so each cell carries its own color.
progressBar :: Demo t m => m (Event t ())
progressBar = do
  t0 <- liftIO getCurrentTime
  tick <- tickLossy 0.04 t0
  pct <- foldDyn (\_ p -> if p >= 1 then 0 else min 1 (p + 0.008)) (0 :: Double) tick
  dw <- displayWidth
  tellImages $ (\w p -> [barImage w p]) <$> current dw <*> current pct
  quit
  where
    grad = gradient1D [(0, RGB 255 70 90), (0.5, RGB 250 200 60), (1, RGB 80 220 120)]
    barImage w p =
      let barW = max 1 (w - 7)
          filled = round (p * fromIntegral barW) :: Int
          cellColor i = fromRGB $ sampleGradient1D grad (fromIntegral i / fromIntegral (max 1 (barW - 1)))
          cell i
            | i < filled = V.char (V.withForeColor V.defAttr (cellColor i)) '█'
            | otherwise = V.char (V.withForeColor V.defAttr (rgbColor 64 64 78)) '░'
          bar = V.horizCat $ map cell [0 .. barW - 1]
          label = V.text' (V.withForeColor V.defAttr accent) $ T.pack $ printf " %3d%%" (round (p * 100) :: Int)
      in V.horizCat [bar, label]
