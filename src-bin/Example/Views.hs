-- |
--   Description: Switching between views, à la the composable-views example.
module Example.Views (views) where

import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | Two views toggled with Tab. Each is just a different rendering chosen from
-- a 'Dynamic' index — the FRP way to swap "views".
views :: Demo t m => m (Event t ())
views = do
  tab <- key (V.KChar '\t')
  n <- foldDyn (\_ i -> (i + 1) `mod` 2) (0 :: Int) tab
  tellImages $ current $ ffor n viewImage
  quit
  where
    hint = V.text' (V.withForeColor V.defAttr dim) "  Tab to switch views · q to quit"
    viewImage 0 =
      [ V.vertCat
          [ V.text' (V.withStyle (V.withForeColor V.defAttr accent) V.bold) "  ◖ View one ◗"
          , V.text' V.defAttr "  reactive, composable terminal UIs"
          , V.char V.defAttr ' '
          , hint
          ]
      ]
    viewImage _ =
      [ V.vertCat
          [ V.text' (V.withStyle (V.withForeColor V.defAttr (rgbColor 200 120 255)) V.bold) "  ◆ View two ◆"
          , V.text' V.defAttr "  swapped with a single keystroke"
          , V.char V.defAttr ' '
          , hint
          ]
      ]
