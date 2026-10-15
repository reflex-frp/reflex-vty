{-# LANGUAGE PackageImports #-}

module Example.TierA (runDemo) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Graphics.Vty as V
import Reflex
import Data.Functor (void)

import "reflex-vty" Reflex.Vty
import Reflex.Vty.Widget.Text (text)
import Reflex.Vty.Widget.Layout

import "reflex-vty-widgets" Reflex.Vty.Widget

-- | Interactive Tier A demonstration. One screen with generated help
-- for the increment/remap/enable bindings (via 'keyHelp'), the grid,
-- the paginator, and the dialog/popover controls. @q@ exits from the
-- background layer only; Ctrl+C is handled by the host as the
-- documented root handler and exits from any state.
runDemo :: IO ()
runDemo =
  mainWidget def $ do
    enterAlternateScreen
    quitE <- quitEvents
    initManager_ $
      col $ do
        grout flex $ keyHelp (pure demoKeys)
        grout flex $ text (pure (T.pack "Tier A demo: keys, grid, paginator, overlays"))
        pure quitE

-- q exits only where the background layer owns input; Ctrl+C is
-- delivered as a host signal and never reaches this handler, so it
-- works from any state.
quitEvents :: (Reflex t, Monad m, HasInput t m) => m (Event t ())
quitEvents = do
  q <- key (V.KChar 'q')
  -- In raw mode Ctrl+C arrives as a key event; handle it here so it
  -- exits from any state, matching the documented root handler.
  i <- input
  let ctrlC = fforMaybe i $ \case
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  pure (leftmost [void q, ctrlC])

demoKeys :: KeyMap ()
demoKeys =
  KeyMap
    [ KeyBinding
        { _keyBinding_action = ()
        , _keyBinding_keys = [(V.KChar 'x', [])]
        , _keyBinding_help = "increment counter (r remaps to i)"
        , _keyBinding_enabled = True
        }
    , KeyBinding
        { _keyBinding_action = ()
        , _keyBinding_keys = [(V.KChar 'r', [])]
        , _keyBinding_help = "remap increment x<->i"
        , _keyBinding_enabled = True
        }
    , KeyBinding
        { _keyBinding_action = ()
        , _keyBinding_keys = [(V.KChar 'd', [])]
        , _keyBinding_help = "enable/disable increment"
        , _keyBinding_enabled = True
        }
    ]

_unusedText :: Text
_unusedText = T.empty
