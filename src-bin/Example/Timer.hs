-- |
--   Description: A countdown timer, à la bubbles\/timer.
module Example.Timer (timer) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex
import Text.Printf (printf)

import Example.Common
import Reflex.Vty

-- | Counts down from ten seconds, ticking every 100ms.
timer :: Demo t m => m (Event t ())
timer = do
  t0 <- liftIO getCurrentTime
  tick <- tickLossy 0.1 t0
  remaining <- foldDyn (\_ r -> max 0 (r - 0.1)) (10 :: Double) tick
  tellImages $ current $ ffor remaining line
  quit
  where
    line r =
      [ V.horizCat
          [ V.text' (V.withForeColor V.defAttr accent) "  ⏳  "
          , V.text' V.defAttr $ T.pack $ printf "%.1fs" r
          , V.text' (V.withForeColor V.defAttr dim) $
              if r <= 0 then "   time's up!  (q to quit)" else "   remaining  (q to quit)"
          ]
      ]
