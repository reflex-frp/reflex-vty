-- |
--   Description: Mouse clicks, drags, and scrolls
module Reflex.Vty.Widget.Input.Mouse where

import Control.Monad.Fix
import qualified Graphics.Vty as V
import Reflex

import Reflex.Vty.Widget

-- | The phase of a drag operation. A drag is a sequence of events that begins
-- with a 'DragStart' (the button is pressed), continues with zero or more
-- 'Dragging' events (the mouse moves with the button held), and finishes with
-- a 'DragEnd' (the button is released).
data DragState
  = -- | The button was just pressed; this is the first event of the drag.
    DragStart
  | -- | The mouse is moving with the button held down.
    Dragging
  | -- | The button was released; this is the last event of the drag.
    DragEnd
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Information about a drag operation
data Drag = Drag
  { _drag_from :: (Int, Int)
  -- ^ Where the drag began
  , _drag_to :: (Int, Int)
  -- ^ Where the mouse currently is
  , _drag_button :: V.Button
  -- ^ Which mouse button is dragging
  , _drag_modifiers :: [V.Modifier]
  -- ^ What modifiers are held
  , _drag_state :: DragState
  -- ^ Which phase of the drag this event represents. Use this to tell the
  -- start of a drag ('DragStart') apart from its continuation ('Dragging')
  -- and its end ('DragEnd').
  }
  deriving (Eq, Ord, Show)

-- | Pure state transition driving 'drag'. Given the button being tracked, the
-- previous 'Drag' state (if any), and an incoming vty event, produce the next
-- 'Drag' or 'Nothing' if the event is irrelevant. The first relevant mouse-down
-- yields a 'DragStart', subsequent moves yield 'Dragging', and the button
-- release yields 'DragEnd'; a mouse-down after a 'DragEnd' starts a fresh drag.
stepDrag :: V.Button -> Maybe Drag -> V.Event -> Maybe Drag
stepDrag btn = \case
  Nothing -> \case
    V.EvMouseDown x y btn' mods
      | btn == btn' -> Just $ Drag (x, y) (x, y) btn' mods DragStart
      | otherwise -> Nothing
    _ -> Nothing
  Just (Drag from _ _ mods st) -> \case
    V.EvMouseDown x y btn' mods'
      | st == DragEnd && btn == btn' -> Just $ Drag (x, y) (x, y) btn' mods' DragStart
      | btn == btn' -> Just $ Drag from (x, y) btn mods' Dragging
      | otherwise -> Nothing -- Ignore other buttons.
    V.EvMouseUp x y (Just btn')
      | st == DragEnd -> Nothing
      | btn == btn' -> Just $ Drag from (x, y) btn mods DragEnd
      | otherwise -> Nothing
    V.EvMouseUp x y Nothing -- Terminal doesn't specify mouse up button,
    -- assume it's the right one.
      | st == DragEnd -> Nothing
      | otherwise -> Just $ Drag from (x, y) btn mods DragEnd
    _ -> Nothing

-- | Converts raw vty mouse drag events into an event stream of 'Drag's
drag
  :: (Reflex t, MonadFix m, MonadHold t m, HasInput t m)
  => V.Button
  -> m (Event t Drag)
drag btn = do
  inp <- input
  rec let newDrag = attachWithMaybe (stepDrag btn) (current dragD) inp
      dragD <- holdDyn Nothing $ Just <$> newDrag
  return (fmapMaybe id $ updated dragD)

-- | Mouse down events for a particular mouse button
mouseDown
  :: (Reflex t, Monad m, HasInput t m)
  => V.Button
  -> m (Event t MouseDown)
mouseDown btn = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvMouseDown x y btn' mods ->
      if btn == btn'
        then Just $ MouseDown btn' (x, y) mods
        else Nothing
    _ -> Nothing

-- | Mouse up events for a particular mouse button
mouseUp
  :: (Reflex t, Monad m, HasInput t m)
  => m (Event t MouseUp)
mouseUp = do
  i <- input
  return $ fforMaybe i $ \case
    V.EvMouseUp x y btn' -> Just $ MouseUp btn' (x, y)
    _ -> Nothing

-- | Information about a mouse down event
data MouseDown = MouseDown
  { _mouseDown_button :: V.Button
  , _mouseDown_coordinates :: (Int, Int)
  , _mouseDown_modifiers :: [V.Modifier]
  }
  deriving (Eq, Ord, Show)

-- | Information about a mouse up event
data MouseUp = MouseUp
  { _mouseUp_button :: Maybe V.Button
  , _mouseUp_coordinates :: (Int, Int)
  }
  deriving (Eq, Ord, Show)

-- | Mouse scroll direction
data ScrollDirection = ScrollDirection_Up | ScrollDirection_Down
  deriving (Eq, Ord, Show)

-- | Produce an event that fires when the mouse wheel is scrolled
mouseScroll
  :: (Reflex t, Monad m, HasInput t m)
  => m (Event t ScrollDirection)
mouseScroll = do
  up <- mouseDown V.BScrollUp
  down <- mouseDown V.BScrollDown
  return $
    leftmost
      [ ScrollDirection_Up <$ up
      , ScrollDirection_Down <$ down
      ]

-- | Track the last known mouse position from any mouse event (down or up).
-- Note: true hover (motion without a button held) requires terminal mode
-- 1003, which vty 6.2 does not expose. Position updates only on click/release.
mousePosition
  :: (Reflex t, MonadHold t m, HasInput t m)
  => m (Dynamic t (Int, Int))
mousePosition = do
  inp <- input
  let posEvent = fforMaybe inp $ \case
        V.EvMouseDown x y _ _ -> Just (x, y)
        V.EvMouseUp x y _ -> Just (x, y)
        _ -> Nothing
  holdDyn (0, 0) posEvent
