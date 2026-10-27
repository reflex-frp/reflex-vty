-- |
-- Module: Reflex.Vty.Widget.List
-- Description: A keyed, selectable, scrollable single-column list
--
-- A 'selectList' renders one row per key of an 'OMap', in the order the
-- map preserves, and owns the selection, keyboard navigation,
-- scrolling, and click handling for those rows.
--
-- The caller provides the region the list may use (usually by running
-- the list inside 'Core.grout' or 'Core.tile'); the list takes its
-- allotted height from 'Core.displayHeight' and never draws outside
-- that region. The row renderer draws what it chooses; the list itself
-- draws nothing.
module Reflex.Vty.Widget.List
  ( SelectListConfig (..)
  , SelectList (..)
  , selectList
  ) where

import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Default (Default (..))
import Data.List (elemIndex)
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Graphics.Vty as V
import Reflex
import qualified Reflex.Vty as Core

-- | Configuration for a select list. The default never requests a
-- selection change.
newtype SelectListConfig t k = SelectListConfig
  { _selectListConfig_setSelection :: Event t k
  -- ^ Select a key when it is present; ignored when absent.
  }

instance Reflex t => Default (SelectListConfig t k) where
  def =
    SelectListConfig
      { _selectListConfig_setSelection = never
      }

-- | The live result of a select list.
data SelectList t k = SelectList
  { _selectList_selection :: Dynamic t (Maybe k)
  -- ^ The currently selected key, if any.
  , _selectList_activate :: Event t k
  -- ^ Fires for Enter, carrying the current selection.
  }

-- | Internal state: the current key order, selection, scroll offset
-- (in rows), and the last known display height.
data ListState k = ListState
  { _listState_keys :: [k]
  , _listState_selection :: Maybe k
  , _listState_offset :: Int
  , _listState_height :: Int
  }

-- | The offset that keeps the selected row inside
-- @[offset, offset + height)@, without moving it when it already is.
ensureVisible :: Ord k => ListState k -> ListState k
ensureVisible st =
  let ks = _listState_keys st
      n = length ks
      h = _listState_height st
      off0 = clampOffset n h (_listState_offset st)
      off1 = case _listState_selection st >>= (`elemIndex` ks) of
        Just i
          | h > 0 && i < off0 -> i
          | h > 0 && i >= off0 + h -> max 0 (i - h + 1)
        _ -> off0
  in st {_listState_offset = clampOffset n h off1}

-- | Clamp an offset so there is no unnecessary empty space below the
-- rows. A height below one clamps as if one row were visible; nothing
-- is drawn in a zero-height list.
clampOffset :: Int -> Int -> Int -> Int
clampOffset n h off = max 0 (min (max 0 (n - max 1 h)) off)

selectKey :: Ord k => k -> ListState k -> ListState k
selectKey k st = ensureVisible (st {_listState_selection = Just k})

-- | Apply a new item map (and the current height). The selection
-- follows its key identity; a vanished key reselects the nearest
-- following survivor, else the nearest preceding survivor, else
-- nothing. An empty-to-nonempty transition selects the first key.
applyItems :: Ord k => Int -> OMap k v -> ListState k -> ListState k
applyItems h om st =
  ensureVisible
    ( st
        { _listState_keys = ks
        , _listState_selection = selection'
        , _listState_height = h
        }
    )
  where
    ks = map fst (OMap.assocs om)
    selection' = case _listState_selection st of
      Nothing -> listToMaybe ks
      Just k
        | k `elem` ks -> Just k
        | otherwise -> nearestSurviving k
    nearestSurviving k =
      let old = _listState_keys st
          (before, after) = case elemIndex k old of
            Nothing -> ([], [])
            Just ix -> (reverse (take ix old), drop (ix + 1) old)
          present x = x `elem` ks
      in case filter present after of
           (k' : _) -> Just k'
           [] -> listToMaybe (filter present before)

-- | React to a new allotted height by keeping the selection visible.
applyHeight :: Ord k => Int -> ListState k -> ListState k
applyHeight h st = ensureVisible (st {_listState_height = h})

-- | Move the selection by @delta@ rows, saturating at both ends.
applyMove :: Ord k => Int -> ListState k -> ListState k
applyMove delta st = case _listState_keys st of
  [] -> st
  ks@(k0 : _) -> case _listState_selection st >>= (`elemIndex` ks) of
    Nothing -> selectKey k0 st
    Just i ->
      let i' = max 0 (min (length ks - 1) (i + delta))
      in selectKey (ks !! i') st

-- | Honor an explicit selection only when the key is present.
applySelect :: Ord k => k -> ListState k -> ListState k
applySelect k st
  | k `elem` _listState_keys st = selectKey k st
  | otherwise = st

-- | Select the key under a visible row (@y@ is list-local).
applyClick :: Ord k => Int -> ListState k -> ListState k
applyClick y st
  | y < 0 = st
  | otherwise =
      let ks = _listState_keys st
          i = _listState_offset st + y
      in if i < length ks then selectKey (ks !! i) st else st

-- | A single-column selectable list over a live ordered map.
--
-- Rows are one display line high and are laid out in 'OMap' key order
-- (the order the map preserves). Only rows inside the current height
-- window are given area; scrolled-out rows are mounted at zero height.
-- The internal offset always keeps the selected row inside
-- @[offset, offset + height)@ and reacts to height changes.
--
-- * The initial selection is the first key of a nonempty map. An empty
--   map has no selection; when an empty map becomes nonempty, the
--   first key is selected.
-- * Up and Down (no modifiers) move the selection by one row,
--   saturating at both ends. They are removed from the input that
--   reaches the row renderers.
-- * Enter (no modifiers) fires '_selectList_activate' for the current
--   selection and is removed from the renderers' input. With no
--   selection it does nothing and fires nothing.
-- * A left mouse press (or release) on a visible row selects it.
-- * Selection tracks the key, not its position: inserting or
--   reordering other keys leaves the selected key selected. If the
--   selected key disappears, the nearest following surviving key is
--   selected, else the nearest preceding one, else nothing.
-- * '_selectListConfig_setSelection' is honored only when its key is
--   present at that moment; absent keys are ignored.
-- * The renderer receives the key, a dynamic value (kept current while
--   the key remains), and whether the row is selected. No highlight is
--   imposed.
--
-- Item-map updates remount the row renderers, because keys and their
-- order may have changed; selection updates and height changes do not.
selectList
  :: forall t m k v
   . ( Reflex t
     , Ord k
     , MonadHold t m
     , MonadFix m
     , Adjustable t m
     , PostBuild t m
     , Core.HasInput t m
     , Core.HasDisplayRegion t m
     , Core.HasImageWriter t m
     , Core.HasFocusReader t m
     , Core.HasLayout t m
     )
  => SelectListConfig t k
  -> Dynamic t (OMap k v)
  -> (k -> Dynamic t v -> Dynamic t Bool -> m ())
  -> m (SelectList t k)
selectList cfg itemsD render = do
  i <- Core.input
  hD <- Core.displayHeight
  pb <- getPostBuild
  let moveE =
        leftmost
          [ (-1) <$ ffilter (== V.EvKey V.KUp []) i
          , 1 <$ ffilter (== V.EvKey V.KDown []) i
          ]
      enterE = () <$ ffilter (== V.EvKey V.KEnter []) i
      clickE = fforMaybe i $ \case
        V.EvMouseDown _ y V.BLeft _ -> Just y
        V.EvMouseUp _ y (Just V.BLeft) -> Just y
        _ -> Nothing
      isListKey = \case
        V.EvKey k [] -> k `elem` [V.KUp, V.KDown, V.KEnter]
        _ -> False
      -- The same consumption mechanism as 'withKeyMap': matched keys
      -- are removed from what the row widgets receive.
      rowInput = ffilter (not . isListKey) i
      itemsF = uncurry applyItems <$> attachWith (,) (current hD) (updated itemsD)
      heightF = applyHeight <$> updated hD
      -- The initial height is read after build: sampling the layout
      -- region during construction demands the layout knot and
      -- deadlocks. Rows already take their visibility from 'hD'
      -- directly, so only the scroll state needs this seed.
      initialHeightF = attachWith (\h () -> applyHeight h) (current hD) pb
      moveF = applyMove <$> moveE
      selectF = applySelect <$> _selectListConfig_setSelection cfg
      clickF = applyClick <$> clickE
      -- Items (then height) first, navigation afterwards; an explicit
      -- selection in the same frame wins over navigation.
      stateE =
        mergeWith
          (flip (.))
          [itemsF, heightF, initialHeightF, moveF, selectF, clickF]
  items0 <- sample (current itemsD)
  let keys0 = map fst (OMap.assocs items0)
      state0 = ensureVisible (ListState keys0 (listToMaybe keys0) 0 0)
  stateD <- foldDyn ($) state0 stateE
  let selectionD = _listState_selection <$> stateD
      offsetD = _listState_offset <$> stateD
      activateE =
        attachWithMaybe
          (\st () -> _listState_selection st)
          (current stateD)
          enterE
      buildRows snapshot =
        Core.axis (pure Core.Orientation_Column) Core.flex $
          forM_ (zip [0 ..] (OMap.assocs snapshot)) $ \(ix, (k, v0)) -> do
            let valueD = fromMaybe v0 . OMap.lookup k <$> itemsD
                selectedD = (== Just k) <$> selectionD
                visibleD =
                  (\off h -> ix >= off && ix < off + h) <$> offsetD <*> hD
            Core.grout
              (Core.fixed (bool 0 1 <$> visibleD))
              (render k valueD selectedD)
  _ <-
    Core.localInput (const rowInput) $
      runWithReplace
        (buildRows items0)
        (buildRows <$> updated itemsD)
  pure
    SelectList
      { _selectList_selection = selectionD
      , _selectList_activate = activateE
      }
