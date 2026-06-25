-- |
--   Description: A scrollable pager, à la bubbles\/viewport (pager example).
module Example.Pager (pager) where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Reflex

import Example.Common
import Reflex.Vty

-- | A 'scrollableText' viewport over a block of numbered lines. Scroll it with
-- the arrow keys or the mouse wheel.
pager :: Demo t m => m (Event t ())
pager = do
  tellImages $
    pure [V.text' (V.withForeColor V.defAttr accent) "  ↑/↓ or mouse wheel to scroll · q to quit"]
  dw <- displayWidth
  dh <- displayHeight
  let viewport w h = Region 0 2 w (max 1 (h - 2))
  _ <- pane (viewport <$> dw <*> dh) (pure True) $ scrollableText def (pure body)
  quit
  where
    body =
      T.unlines
        [ "Line " <> T.pack (show n) <> " — scrollable viewport content"
        | n <- [1 .. 60 :: Int]
        ]
