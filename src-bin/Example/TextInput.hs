-- |
--   Description: A single text input field, à la bubbles\/textinput.
module Example.TextInput (textInputExample) where

import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | A prompt and a 'textInput' below it. The input lives in an always-focused
-- 'pane', so it accepts typing immediately. Quit with Ctrl+C (q is a character).
textInputExample :: Demo t m => m (Event t ())
textInputExample = do
  dw <- displayWidth
  tellImages $
    pure
      [ V.vertCat
          [ V.text' (V.withForeColor V.defAttr accent) "  What's your name?"
          , V.text' V.defAttr ""
          ]
      ]
  let inputRegion w = Region 2 2 (max 1 (w - 4)) 1
  _ <- pane (inputRegion <$> dw) (pure True) $ textInput def
  tellImages $
    pure [V.translate 2 4 $ V.text' (V.withForeColor V.defAttr dim) "(type away — Ctrl+C to quit)"]
  ctrlc
