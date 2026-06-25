-- |
--   Description: An animated splash screen, à la the splash example.
module Example.Splash (splash) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | A centered title that types out in a pink→violet→cyan gradient, with a
-- subtitle that fades in beneath it; the whole thing loops.
splash :: Demo t m => m (Event t ())
splash = do
  t0 <- liftIO getCurrentTime
  tick <- tickLossy 0.06 t0
  n <- foldDyn (\_ i -> i + 1) (0 :: Int) tick
  dw <- displayWidth
  dh <- displayHeight
  tellImages $ (\w h i -> [frame w h i]) <$> current dw <*> current dh <*> current n
  quit
  where
    title = "reflex-vty"
    subtitle = "reactive terminal UIs"
    grad = gradient1D [(0, RGB 255 64 160), (0.5, RGB 150 90 255), (1, RGB 0 200 230)]
    frame w h i =
      let p = i `mod` 95
          revealed = min (length title) (max 0 (p `div` 2))
          subAlpha = max 0 (min 1 (fromIntegral (p - 26) / 18 :: Double))
          pad k = V.text' V.defAttr (T.replicate (max 0 k) " ")
          center len = (w - len) `div` 2
          titleImg =
            V.horizCat
              [ V.char (V.withStyle (V.withForeColor V.defAttr (charColor k)) V.bold) c
              | (k, c) <- zip [0 :: Int ..] (take revealed title)
              ]
          charColor k = fromRGB $ sampleGradient1D grad (fromIntegral k / fromIntegral (max 1 (length title - 1)))
          subImg
            | subAlpha <= 0 = V.emptyImage
            | otherwise = V.text' (V.withForeColor V.defAttr (fromRGB (mix subAlpha (RGB 35 35 45) (RGB 150 150 170)))) (T.pack subtitle)
          gap = V.char V.defAttr ' '
          top = max 0 (h `div` 2 - 1)
      in V.vertCat $
           replicate top gap
             ++ [ V.horizCat [pad (center (length title)), titleImg]
                , gap
                , V.horizCat [pad (center (length subtitle)), subImg]
                ]
