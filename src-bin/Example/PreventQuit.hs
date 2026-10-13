{-# LANGUAGE LambdaCase #-}

-- |
--   Description: Ask for confirmation before quitting, a la the Bubble Tea
--   prevent-quit example.
module Example.PreventQuit (preventQuit) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | Requests to quit (q or Ctrl+C) open a confirmation view. Only an
-- unmodified y or Y confirms; n, N, Enter, or Esc returns to the running view.
-- The returned event is the shutdown request; 'Example.Common.quit' here is
-- only the confirmation request.
preventQuit :: Demo t m => m (Event t ())
preventQuit = do
  requested <- quit
  yes <- keys [V.KChar 'y', V.KChar 'Y']
  no <- keys [V.KChar 'n', V.KChar 'N', V.KEnter, V.KEsc]
  confirming <- holdDyn False $ leftmost [True <$ requested, False <$ no]
  tellImages $ current $ ffor confirming view
  pure $ () <$ gate (current confirming) yes
  where
    view True =
      [ V.vertCat
          [ V.text' (V.withStyle (V.withForeColor V.defAttr accent) V.bold) (T.pack "  Quit confirmation")
          , V.char V.defAttr ' '
          , V.text' V.defAttr (T.pack "  Really quit? [y/N]")
          , V.char V.defAttr ' '
          , V.text' (V.withForeColor V.defAttr dim) (T.pack "  y: quit; n, Enter, or Esc: return")
          ]
      ]
    view False =
      [ V.vertCat
          [ V.text' (V.withStyle (V.withForeColor V.defAttr accent) V.bold) (T.pack "  Quit confirmation")
          , V.char V.defAttr ' '
          , V.text' V.defAttr (T.pack "  Application is running.")
          , V.char V.defAttr ' '
          , V.text' (V.withForeColor V.defAttr dim) (T.pack "  q or Ctrl+C: request exit")
          ]
      ]
