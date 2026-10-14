-- |
--   Description: A five-second countdown that exits on its own, a la the
--   Bubble Tea simple example.
module Example.Simple (simple) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Time (tickLossyFrom')

import Example.Common
import Reflex.Vty

-- | Counts down from five seconds and requests shutdown at zero. Remaining
-- time comes from the monotonic clock at each one-second tick, so delayed
-- processing skips expired values instead of stretching the countdown. The
-- timer metadata from 'tickLossyFrom'' is computed before sleeping, so the
-- first tick carries '_tickInfo_n == 0' and must not be subtracted here.
simple :: Demo t m => m (Event t ())
simple = do
  postBuild <- getPostBuild
  -- Monotonic first: the timer's UTC basis must not be earlier than the
  -- countdown start.
  started <- performEvent $ ffor postBuild $ \_ -> do
    startNSec <- liftIO getMonotonicTimeNSec
    startUTC <- liftIO getCurrentTime
    pure (startNSec, startUTC)
  startBehavior <- hold 0 (fst <$> started)
  tick <- tickLossyFrom' ((\(_, utc) -> (1, utc)) <$> started)
  -- Pair each tick with the held start time sampled in the widget monad,
  -- where 'MonadSample' is available, then read the clock inside the
  -- performed action.
  remainingEvents <-
    performEvent $
      attachWith
        ( \startNSec _tickInfo -> do
            now <- liftIO getMonotonicTimeNSec
            pure $ max 0 (5 - fromIntegral ((now - startNSec) `div` 1000000000) :: Int)
        )
        startBehavior
        tick
  remaining <- holdDyn 5 (ffilter (/= 0) remainingEvents)
  earlyQuit <- quit
  tellImages $ current $ ffor remaining $ \r ->
    [ V.vertCat
        [ V.horizCat
            [ V.text' V.defAttr "  This program exits in "
            , V.text' (V.withForeColor V.defAttr accent) (T.pack (show r))
            , V.text' V.defAttr " seconds."
            ]
        , V.char V.defAttr ' '
        , V.text' (V.withForeColor V.defAttr dim) "  q or Ctrl+C: quit sooner"
        ]
    ]
  pure $ leftmost [earlyQuit, () <$ ffilter (== 0) remainingEvents]
