{-|
  Description: Widgets that split the display vertically or horizontally.
-}
module Reflex.Vty.Widget.Split where

import Control.Applicative
import Control.Monad.Fix
import Graphics.Vty as V
import Reflex
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Input.Mouse

-- | A split of the available space into two parts with a draggable separator.
-- Starts with half the space allocated to each, and the first pane has focus.
-- Clicking in a pane switches focus.
splitVDrag :: (Reflex t, MonadFix m, MonadHold t m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m)
  => m ()
  -> m a
  -> m b
  -> m (a,b)
splitVDrag wS wA wB = do
  dh <- displayHeight
  dw <- displayWidth
  h0 <- sample $ current dh -- TODO
  dragE <- drag V.BLeft
  let splitter0 = h0 `div` 2
  rec splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
      splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
      splitterFrac <- holdDyn ((1::Double) / 2) $ ffor (attach (current dh) (fst <$> dragSplitter)) $ \(h, x) ->
        fromIntegral x / max 1 (fromIntegral h)
      let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
            \(splitterY, Drag (_, fromY) (_, toY) _ _ end) ->
              if splitterY == fromY then Just (toY, end) else Nothing
          regA = Region 0 0 <$> dw <*> splitterPos
          regS = Region 0 <$> splitterPos <*> dw <*> 1
          regB = Region 0 <$> (splitterPos + 1) <*> dw <*> (dh - splitterPos - 1)
          resizeSplitter = ffor (attach (current splitterFrac) (updated dh)) $
            \(frac, h) -> round (frac * fromIntegral h)
      focA <- holdDyn True $ leftmost
        [ True <$ mA
        , False <$ mB
        ]
      (mA, rA) <- pane regA focA $ withMouseDown wA
      pane regS (pure False) wS
      (mB, rB) <- pane regB (not <$> focA) $ withMouseDown wB
  return (rA, rB)
  where
    withMouseDown x = do
      m <- mouseDown V.BLeft
      x' <- x
      return (m, x')

-- | A plain split of the available space into vertically stacked panes.
-- No visual separator is built in here.
splitV :: (Reflex t, Monad m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m)
       => Dynamic t (Int -> Int)
       -- ^ Function used to determine size of first pane based on available size
       -> Dynamic t (Bool, Bool)
       -- ^ How to focus the two sub-panes, given that we are focused.
       -> m a
       -- ^ Widget for first pane
       -> m b
       -- ^ Widget for second pane
       -> m (a,b)
splitV sizeFunD focD wA wB = do
  dw <- displayWidth
  dh <- displayHeight
  let regA = Region 0 0 <$> dw <*> (sizeFunD <*> dh)
      regB = Region 0 <$> (_region_height <$> regA) <*> dw <*> liftA2 (-) dh (_region_height <$> regA)
  ra <- pane regA (fst <$> focD) wA
  rb <- pane regB (snd <$> focD) wB
  return (ra,rb)

-- | A plain split of the available space into horizontally stacked panes.
-- No visual separator is built in here.
splitH :: (Reflex t, Monad m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m)
       => Dynamic t (Int -> Int)
       -- ^ Function used to determine size of first pane based on available size
       -> Dynamic t (Bool, Bool)
       -- ^ How to focus the two sub-panes, given that we are focused.
       -> m a
       -- ^ Widget for first pane
       -> m b
       -- ^ Widget for second pane
       -> m (a,b)
splitH sizeFunD focD wA wB = do
  dw <- displayWidth
  dh <- displayHeight
  let regA = Region 0 0 <$> (sizeFunD <*> dw) <*> dh
      regB = Region <$> (_region_width <$> regA) <*> 0 <*> liftA2 (-) dw (_region_width <$> regA) <*> dh
  liftA2 (,) (pane regA (fmap fst focD) wA) (pane regB (fmap snd focD) wB)

