{-# LANGUAGE LambdaCase #-}

-- |
--   Description: Show the raw Vty keyboard event, a la the Bubble Tea
--   print-key example.
module Example.PrintKey (printKey) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | Prints the latest 'V.EvKey' event, including its modifier list. Ordinary
-- characters (including q) are never treated as exit commands here; only
-- Ctrl+C quits. Non-key events (paste, mouse, focus, resize) do not replace
-- the displayed keyboard event.
printKey :: Demo t m => m (Event t ())
printKey = do
  i <- input
  let keyEvents = fforMaybe i $ \case
        e@V.EvKey {} -> Just e
        _ -> Nothing
  lastKey <- holdDyn (T.pack "No key received.") (T.pack . show <$> keyEvents)
  tellImages $ current $ ffor lastKey $ \k ->
    [ V.vertCat
        [ V.text' (V.withStyle (V.withForeColor V.defAttr accent) V.bold) (T.pack "  Keyboard events")
        , V.char V.defAttr ' '
        , V.text' V.defAttr ((T.pack "  ") <> k)
        , V.char V.defAttr ' '
        , V.text' (V.withForeColor V.defAttr dim) (T.pack "  Press a key. Ctrl+C: quit")
        ]
    ]
  ctrlc
