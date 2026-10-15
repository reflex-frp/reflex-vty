module Example.TierA (runDemo) where

import Data.Default (Default (..))
import Reflex
import Reflex.Vty
import qualified Graphics.Vty as V

-- | Interactive Tier A demonstration. Full controls arrive with the
-- grid, paginator, and overlay steps; this provides the shared screen
-- scaffold and alternate-screen setup.
runDemo :: IO ()
runDemo = mainWidget def $ do
  enterAlternateScreen
  pure never
  where
    _unused = ()

_ = def :: Default Padding => Padding
