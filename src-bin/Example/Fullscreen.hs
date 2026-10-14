-- |
--   Description: Countdown on the alternate screen, a la the Bubble Tea
--   fullscreen example.
module Example.Fullscreen (fullscreen) where

import Reflex

import Example.Common (Demo)
import Example.Simple (simple)
import Reflex.Vty

-- | Enters the alternate screen, then runs the five-second countdown from
-- "Example.Simple" unchanged. The host's normal cleanup restores the primary
-- screen on any exit path.
fullscreen :: (Demo t m, HasScreenMode t m) => m (Event t ())
fullscreen = do
  enterAlternateScreen
  simple
