-- |
--   Description: Shared scaffolding for the small standalone example demos.
module Example.Common where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty

-- | The set of capabilities the little single-screen demos rely on. They draw
-- with 'tellImages'\/'text', read the clock via 'tickLossy', and quit on a key.
type Demo t m =
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  , MonadIO m
  , MonadIO (Performable m)
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , HasInput t m
  , HasImageWriter t m
  , HasDisplayRegion t m
  , HasFocusReader t m
  , HasTheme t m
  )

-- | The brand accent color (cyan), matching the rest of the demos.
accent :: V.Color
accent = rgbColor 0 200 230

-- | A dim grey for hints.
dim :: V.Color
dim = rgbColor 120 120 140

-- | An 'Event' that fires when the user asks to quit (q or Ctrl+C). Demos
-- return this from their widget so 'mainWidget' shuts down.
quit :: (Reflex t, Monad m, HasInput t m) => m (Event t ())
quit = do
  q <- key (V.KChar 'q')
  c <- ctrlc
  pure $ leftmost [() <$ q, c]
