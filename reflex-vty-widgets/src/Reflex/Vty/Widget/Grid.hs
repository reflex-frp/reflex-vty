{-# LANGUAGE FlexibleContexts #-}

-- | A fixed-list grid layout with explicit gaps and padding, backed
-- by the core layout solver.
module Reflex.Vty.Widget.Grid
  ( GridConfig (..)
  , grid
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Default (Default (..))
import qualified Data.Map.Ordered as OMap
import qualified Data.Map.Strict as Map
import Reflex
import Reflex.Vty (Region (..), VtyEvent)
import qualified Graphics.Vty as V

import qualified Reflex.Vty as Core
import Reflex.Vty.Style (Padding (..))
import Reflex.Vty.Widget.Layout
  ( Constraint (..)
  , LayoutForest (..)
  , LayoutTree (..)
  , Orientation (..)
  , fromListLF
  , rootLT
  , solve
  )
import Control.Monad.NodeId (NodeId (..))

-- | Grid configuration. Defaults: one column, zero gaps and padding.
data GridConfig t = GridConfig
  { _gridConfig_columns :: Dynamic t Int
  , _gridConfig_columnGap :: Dynamic t Int
  , _gridConfig_rowGap :: Dynamic t Int
  , _gridConfig_padding :: Dynamic t Padding
  }

instance Reflex t => Default (GridConfig t) where
  def =
    GridConfig
      { _gridConfig_columns = pure 1
      , _gridConfig_columnGap = pure 0
      , _gridConfig_rowGap = pure 0
      , _gridConfig_padding = pure def
      }

-- | Lay a fixed list of children out row-major. Children are mounted
-- exactly once: changing columns, gaps, padding, or the viewport size
-- never rebuilds them, resets their state, or reallocates their
-- focus/node IDs.
--
-- No children yields no output and no tracks. The last (incomplete)
-- row keeps the same column widths as every other row. Solved cells
-- are clamped to the content rectangle; fixed gaps may exceed the
-- available extent, in which case later cells have zero area.
--
-- Each child action normally allocates its own 'Core.tile' or
-- 'Core.grout' inside the pane the grid provides; the grid itself
-- registers no focus targets. New keyboard, paste, and mouse-down
-- activity is rejected in a zero-area cell; terminal focus\/resize
-- notifications and the completion of an already-captured mouse
-- gesture still pass.
grid
  :: forall t m a
   . ( Reflex t, MonadFix m, MonadHold t m, Core.MonadNodeId m
     , Core.HasInput t m, Core.HasImageWriter t m, Core.HasDisplayRegion t m
     , Core.HasFocusReader t m
     )
  => GridConfig t
  -> [m a]
  -> m [a]
grid cfg kids = case kids of
  [] -> pure []
  _ -> do
    vp <- Core.askViewport
    let childCount = length kids
        columnsD = clampColumns childCount <$> _gridConfig_columns cfg
        colGapD = max 0 <$> _gridConfig_columnGap cfg
        rowGapD = max 0 <$> _gridConfig_rowGap cfg
        padD = clampPad <$> _gridConfig_padding cfg
    -- Allocate track node IDs once, sufficient for the maximum rows
    -- and columns; gaps interleave using the next track's id slot.
    colIds <- mapM (const Core.getNextNodeId) [1 .. 2 * childCount]
    rowIds <- mapM (const Core.getNextNodeId) [1 .. 2 * childCount]
    let cellsD =
          solveGrid
            <$> vp
            <*> columnsD
            <*> colGapD
            <*> rowGapD
            <*> padD
            <*> pure childCount
            <*> pure colIds
            <*> pure rowIds
    mapM (runChild cellsD) (zip [0 ..] kids)
  where
    clampColumns n c = max 1 (min n c)
    clampPad p =
      Padding
        { _padding_top = max 0 (_padding_top p)
        , _padding_bottom = max 0 (_padding_bottom p)
        , _padding_left = max 0 (_padding_left p)
        , _padding_right = max 0 (_padding_right p)
        }
    runChild cellsD (i, child) = do
      let rectD = maybe nilRegion id . Map.lookup i <$> cellsD
          hasArea = (\r -> _region_width r > 0 && _region_height r > 0) <$> rectD
      -- New keyboard/paste/mouse-down is rejected in a zero-area cell;
      -- other events (focus/resize notifications, releases completing
      -- an already-captured gesture) pass unchanged.
      inp <- Core.input
      let gatedInput =
            fmapMaybe id $
              pushAlways
                ( \e -> do
                    ok <- sample (current hasArea)
                    pure $ case e of
                      V.EvKey {} -> if ok then Just e else Nothing
                      V.EvPaste {} -> if ok then Just e else Nothing
                      V.EvMouseDown {} -> if ok then Just e else Nothing
                      _ -> Just e
                )
                inp
      Core.localInput (const gatedInput) (Core.pane rectD (pure True) child)
    nilRegion = Region 0 0 0 0

-- Compute every cell rectangle for one frame.
solveGrid
  :: Region
  -> Int -- columns
  -> Int -- column gap
  -> Int -- row gap
  -> Padding
  -> Int -- child count
  -> [NodeId] -- column axis id slots (2 per track+gap)
  -> [NodeId] -- row axis id slots
  -> Map.Map Int Region
solveGrid vp cols colGap rowGap pad childCount colIds rowIds =
  let w = _region_width vp
      h = _region_height vp
      left = min w (_padding_left pad)
      top = min h (_padding_top pad)
      cw = max 0 (w - left - min (w - left) (_padding_right pad))
      ch = max 0 (h - top - min (h - top) (_padding_bottom pad))
      -- Parent-pane-local coordinates: the caller's 'Core.pane' adds
      -- the viewport origin exactly once when composing cell panes.
      content = Region left top cw ch
      rows = 1 + ((childCount - 1) `div` cols)
      colRegions = trackRegions Orientation_Row content cols colGap cw (take cols colIds) (drop cols colIds)
      rowRegions = trackRegions Orientation_Column content rows rowGap ch (take rows rowIds) (drop rows rowIds)
      clampCell r =
        Region
          (max (_region_left content) (_region_left r))
          (max (_region_top content) (_region_top r))
          (max 0 (min cw (_region_width r)))
          (max 0 (min ch (_region_height r)))
   in Map.fromList
        [ (i, clampCell (Region x y wid hei))
        | i <- [0 .. childCount - 1]
        , let r = i `div` cols
        , let c = i `mod` cols
        , let xr = colRegions !! c
        , let yr = rowRegions !! r
        , let x = _region_left xr
        , let wid = _region_width xr
        , let y = _region_top yr
        , let hei = _region_height yr
        ]

-- Solve one axis over a content rectangle using the core 'solve':
-- alternating Min-0 tracks and Fixed gaps, then return the rectangle
-- of each track. Track ids and gap ids are preallocated.
trackRegions
  :: Orientation
  -> Region
  -> Int -- track count
  -> Int -- gap size
  -> Int -- extent along the axis
  -> [NodeId] -- track ids
  -> [NodeId] -- gap ids (at least trackCount - 1)
  -> [Region]
trackRegions o content n gap extent trackIds gapIds
  | n <= 0 = []
  | otherwise =
      let elements =
            concat
              [ (if k == 0 then [] else [(gapId (k - 1), (Constraint_Fixed gap, Orientation_Row))])
                  ++ [(trackId k, (Constraint_Min 0, Orientation_Row))]
              | k <- [0 .. n - 1]
              ]
          forest =
            fromListLF
              [ (i, LayoutTree c mempty)
              | (i, c) <- elements
              ]
          solved = solve o content forest
          trackRegion k =
            case lookupLFId (trackId k) solved of
              Just t -> slice (fst (rootLT t))
              Nothing -> nil
          slice r = case o of
            Orientation_Row ->
              Region
                (_region_left r)
                (_region_top content)
                (_region_width r)
                (_region_height content)
            Orientation_Column ->
              Region
                (_region_left content)
                (_region_top r)
                (_region_width content)
                (_region_height r)
       in map trackRegion [0 .. n - 1]
  where
    trackId k = trackIds !! k
    gapId k = gapIds !! k
    nil = Region 0 0 0 0
    lookupLFId i (LayoutTree _ (LayoutForest kids)) =
      case [t | (i', t) <- OMap.assocs kids, i' == i] of
        (t : _) -> Just t
        [] -> Nothing
