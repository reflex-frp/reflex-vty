-- |
--   Description: A count-up stopwatch, à la bubbles\/stopwatch.
module Example.Stopwatch (stopwatch) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Reflex
import Text.Printf (printf)

import Example.Common
import Reflex.Vty

-- | Counts up from zero, ticking every 100ms.
stopwatch :: Demo t m => m (Event t ())
stopwatch = do
  t0 <- liftIO getCurrentTime
  tick <- tickLossy 0.1 t0
  elapsed <- foldDyn (\_ d -> d + 0.1) (0 :: Double) tick
  tellImages $ current $ ffor elapsed line
  quit
  where
    line d =
      [ V.horizCat
          [ V.text' (V.withForeColor V.defAttr accent) "  ⏱  "
          , V.text' V.defAttr $ T.pack $ printf "%.1fs" d
          , V.text' (V.withForeColor V.defAttr dim) "   (q to quit)"
          ]
      ]
