{-|
Module: Reflex.Vty.Widget.Layout
Description: Monad transformer and tools for arranging widgets and building screen layouts
-}
{-# Language UndecidableInstances #-}

module Reflex.Vty.Widget.Layout where

import Control.Applicative (liftA2)
import Control.Monad.Morph
import Control.Monad.NodeId (MonadNodeId(..), NodeId)
import Control.Monad.Reader
import Data.List (mapAccumL)
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import Data.Maybe (fromMaybe, isNothing)
import Data.Ratio ((%))
import Data.Semigroup (First(..))
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import qualified Graphics.Vty as V

import Reflex
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Vty.Widget

-- * Focus
--
-- $focus
--
-- The focus monad tracks which element is currently focused and processes
-- requests to change focus. Focusable elements are assigned a 'FocusId' and
-- can manually request focus or receive focus due to some other action (e.g.,
-- a tab press in a sibling element, a click event).
--
-- Focusable elements will usually be created via 'tile', but can also be
-- constructed via 'makeFocus' in 'MonadFocus'. The latter option allows for
-- more find-grained control of focus behavior.

-- ** Storing focus state

-- | Identifies an element that is focusable. Can be created using 'makeFocus'.
newtype FocusId = FocusId NodeId
  deriving (Eq, Ord)

-- | An ordered set of focus identifiers. The order here determines the order
-- in which focus cycles between focusable elements.
newtype FocusSet = FocusSet { unFocusSet :: OSet FocusId }

instance Semigroup FocusSet where
  FocusSet a <> FocusSet b = FocusSet $ a OSet.|<> b

instance Monoid FocusSet where
  mempty = FocusSet OSet.empty

-- | Produces a 'FocusSet' with a single element
singletonFS :: FocusId -> FocusSet
singletonFS = FocusSet . OSet.singleton

-- ** Changing focus state

-- | Operations that change the currently focused element.
data Refocus = Refocus_Shift Int -- ^ Shift the focus by a certain number of positions (see 'shiftFS')
             | Refocus_Id FocusId -- ^ Focus a particular element
             | Refocus_Clear -- ^ Remove focus from all elements

-- | Given a 'FocusSet', a currently focused element, and a number of positions
-- to move by, determine the newly focused element.
shiftFS :: FocusSet -> Maybe FocusId -> Int -> Maybe FocusId
shiftFS (FocusSet s) fid n = case OSet.findIndex <$> fid <*> pure s of
  Nothing -> OSet.elemAt s 0
  Just Nothing -> OSet.elemAt s 0
  Just (Just ix) -> OSet.elemAt s $ mod (ix + n) (OSet.size s)

-- ** The focus management monad

-- | A class for things that can produce focusable elements.
class (Monad m, Reflex t) => MonadFocus t m | m -> t where
  -- | Create a focusable element.
  makeFocus :: m FocusId
  -- | Emit an 'Event' of requests to change the focus.
  requestFocus :: Event t Refocus -> m ()
  -- | Produce a 'Dynamic' that indicates whether the given 'FocusId' is focused.
  isFocused :: FocusId -> m (Dynamic t Bool)
  -- | Run an action, additionally returning the focusable elements it produced.
  subFoci :: m a -> m (a, Dynamic t FocusSet)
  -- | Get a 'Dynamic' of the currently focused element identifier.
  focusedId :: m (Dynamic t (Maybe FocusId))

-- | A monad transformer that keeps track of the set of focusable elements and
-- which, if any, are currently focused, and allows focus requests.
newtype Focus t m a = Focus
  { unFocus :: DynamicWriterT t FocusSet
      (ReaderT (Dynamic t (Maybe FocusId))
        (EventWriterT t (First Refocus) m)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadHold t
    , MonadSample t
    , MonadFix
    , TriggerEvent t
    , PerformEvent t
    , NotReady t
    , MonadReflexCreateTrigger t
    , HasDisplaySize t
    , PostBuild t
    , MonadNodeId
    , MonadIO
    )


instance MonadTrans (Focus t) where
  lift = Focus . lift . lift . lift

instance MFunctor (Focus t) where
  hoist f = Focus . hoist (hoist (hoist f)) . unFocus

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (Focus t m) where
  runWithReplace (Focus a) e = Focus $ runWithReplace a $ fmap unFocus e
  traverseIntMapWithKeyWithAdjust f m e = Focus $ traverseIntMapWithKeyWithAdjust (\k v -> unFocus $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = Focus $ traverseDMapWithKeyWithAdjust (\k v -> unFocus $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = Focus $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unFocus $ f k v) m e

instance (Reflex t, MonadFix m, HasVtyInput t m) => HasVtyInput t (Focus t m) where
  localInput f = hoist (localInput f)

instance (HasVtyWidgetCtx t m, Reflex t, MonadFix m) => HasVtyWidgetCtx t (Focus t m) where
  localCtx f = hoist (localCtx f)

instance (ImageWriter t m, MonadFix m) => ImageWriter t (Focus t m) where
  mapImages f = hoist (mapImages f)

instance (HasFocus t m, Monad m) => HasFocus t (Focus t m)

instance (Reflex t, MonadFix m, MonadNodeId m) => MonadFocus t (Focus t m) where
  makeFocus = do
    fid <- FocusId <$> lift getNextNodeId
    Focus $ tellDyn $ pure $ singletonFS fid
    pure fid
  requestFocus = Focus . tellEvent . fmap First
  isFocused fid = do
    sel <- Focus ask
    pure $ (== Just fid) <$> sel
  subFoci (Focus child) = Focus $ do
    (a, fs) <- lift $ runDynamicWriterT child
    tellDyn fs
    return (a, fs)
  focusedId = Focus ask

-- | Runs a 'Focus' action, maintaining the selection state internally.
runFocus
  :: (MonadFix m, MonadHold t m, Reflex t)
  => Focus t m a
  -> m (a, Dynamic t FocusSet)
runFocus (Focus x) = do
  rec ((a, focusIds), focusRequests) <- runEventWriterT $ flip runReaderT sel $ runDynamicWriterT x
      sel <- foldDyn f Nothing $ attach (current focusIds) focusRequests
  pure (a, focusIds)
  where
    f :: (FocusSet, First Refocus) -> Maybe FocusId -> Maybe FocusId
    f (fs, rf) mf = case getFirst rf of
      Refocus_Clear -> Nothing
      Refocus_Id fid -> Just fid
      Refocus_Shift n -> if n < 0 && isNothing mf
        then shiftFS fs (OSet.elemAt (unFocusSet fs) 0) n
        else shiftFS fs mf n

-- | Runs an action in the focus monad, providing it with information about
-- whether any of the foci created within it are focused.
anyChildFocused
  :: (MonadFocus t m, MonadFix m)
  => (Dynamic t Bool -> m a)
  -> m a
anyChildFocused f = do
  fid <- focusedId
  rec (a, fs) <- subFoci (f b)
      let b = liftA2 (\foc s -> case foc of
            Nothing -> False
            Just f' -> OSet.member f' $ unFocusSet s) fid fs
  pure a

-- ** Focus controls

-- | Request focus be shifted backward and forward based on tab presses. <Tab>
-- shifts focus forward and <Shift+Tab> shifts focus backward.
tabNavigation :: (Reflex t, MonadNodeId m, HasVtyInput t m, MonadFocus t m) => m ()
tabNavigation = do
  fwd <- fmap (const 1) <$> key (V.KChar '\t')
  back <- fmap (const (-1)) <$> key V.KBackTab
  requestFocus $ Refocus_Shift <$> leftmost [fwd, back]

-- * Layout
--
-- $layout
-- The layout monad keeps track of a tree of elements, each having its own
-- layout constraints and orientation. Given the available rendering space, it
-- computes a layout solution and provides child elements with their particular
-- layout solution (the width and height of their rendering space).
--
-- Complex layouts are built up though some combination of:
--
-- - 'axis', which lays out its children in a particular orientation, and
-- - 'region', which "claims" some part of the screen according to its constraints
--

-- ** Layout restrictions

-- *** Constraints

-- | Datatype representing constraints on a widget's size along the main axis (see 'Orientation')
data Constraint = Constraint_Fixed Int
                | Constraint_Min Int
  deriving (Show, Read, Eq, Ord)

-- | Shorthand for constructing a fixed constraint
fixed
  :: Reflex t
  => Dynamic t Int
  -> Dynamic t Constraint
fixed = fmap Constraint_Fixed

-- | Shorthand for constructing a minimum size constraint
stretch
  :: Reflex t
  => Dynamic t Int
  -> Dynamic t Constraint
stretch = fmap Constraint_Min

-- | Shorthand for constructing a constraint of no minimum size
flex
  :: Reflex t
  => Dynamic t Constraint
flex = pure $ Constraint_Min 0

-- *** Orientation

-- | The main-axis orientation of a 'Layout' widget
data Orientation = Orientation_Column
                 | Orientation_Row
  deriving (Show, Read, Eq, Ord)

-- | Create a row-oriented 'axis'
row
  :: (Reflex t, MonadNodeId m, MonadFix m, MonadLayout t m)
  => m a
  -> m a
row = axis (pure Orientation_Row) flex

-- | Create a column-oriented 'axis'
col
  :: (Reflex t, MonadNodeId m, MonadFix m, MonadLayout t m)
  => m a
  -> m a
col = axis (pure Orientation_Column) flex

-- ** Layout management data

-- | A collection of information related to the layout of the screen. The root
-- node is a "parent" widget, and the contents of the 'LayoutForest' are its
-- children.
data LayoutTree a = LayoutTree a (LayoutForest a)
  deriving (Show)

-- | An ordered, indexed collection of 'LayoutTree's representing information
-- about the children of some widget.
newtype LayoutForest a = LayoutForest { unLayoutForest :: OMap NodeId (LayoutTree a) }
  deriving (Show)

instance Semigroup (LayoutForest a) where
  LayoutForest a <> LayoutForest b = LayoutForest $ a OMap.|<> b

instance Monoid (LayoutForest a) where
  mempty = LayoutForest OMap.empty

-- | Perform a lookup by 'NodeId' in a 'LayoutForest'
lookupLF :: NodeId -> LayoutForest a -> Maybe (LayoutTree a)
lookupLF n (LayoutForest a) = OMap.lookup n a

-- | Create a 'LayoutForest' with one element
singletonLF :: NodeId -> LayoutTree a -> LayoutForest a
singletonLF n t = LayoutForest $ OMap.singleton (n, t)

-- | Produce a 'LayoutForest' from a list. The order of the list is preserved.
fromListLF :: [(NodeId, LayoutTree a)] -> LayoutForest a
fromListLF = LayoutForest . OMap.fromList

-- | Extract the information at the root of a 'LayoutTree'
rootLT :: LayoutTree a -> a
rootLT (LayoutTree a _) = a

-- | Extract the child nodes of a 'LayoutTree'
childrenLT :: LayoutTree a -> LayoutForest a
childrenLT (LayoutTree _ a) = a

-- | Produce a layout solution given a starting orientation, the overall screen
-- size, and a set of constraints.
solve
  :: Orientation
  -> Region
  -> LayoutForest (Constraint, Orientation)
  -> LayoutTree (Region, Orientation)
solve o0 r0 (LayoutForest cs) =
  let a = map (\(x, t@(LayoutTree (c, _) _)) -> ((x, t), c)) $ OMap.assocs cs
      extent = case o0 of
        Orientation_Row -> _region_width r0
        Orientation_Column -> _region_height r0
      sizes = computeEdges $ computeSizes extent a
      chunks = [ (nodeId, solve o1 r1 f)
               | ((nodeId, LayoutTree (_, o1) f), sz) <- sizes
               , let r1 = chunk o0 r0 sz
               ]
  in LayoutTree (r0, o0) $ fromListLF chunks
  where
    computeEdges :: [(a, Int)] -> [(a, (Int, Int))]
    computeEdges = ($ []) . fst . foldl (\(m, offset) (a, sz) ->
      (((a, (offset, sz)) :) . m, sz + offset)) (id, 0)
    computeSizes
      :: Int
      -> [(a, Constraint)]
      -> [(a, Int)]
    computeSizes available constraints =
      -- The minimum amount of space we need. Calculated by adding up all of
      -- the fixed size items and all the minimum sizes of stretchable items
      let minTotal = sum $ ffor constraints $ \case
            (_, Constraint_Fixed n) -> n
            (_, Constraint_Min n) -> n
      -- The leftover space is the area we can allow stretchable items to
      -- expand into
          leftover = max 0 (available - minTotal)
      -- The number of stretchable items that will try to share some of the
      -- leftover space
          numStretch = length $ filter (isMin . snd) constraints
      -- Space to allocate to the stretchable items (this is the same for all
      -- items and there may still be additional leftover space that will have
      -- to be unevenly distributed)
          szStretch = floor $ leftover % max numStretch 1
      -- Remainder of available space after even distribution. This extra space
      -- will be distributed to as many stretchable widgets as possible.
          adjustment = max 0 $ available - minTotal - szStretch * numStretch
      in snd $ mapAccumL (\adj (a, c) -> case c of
          Constraint_Fixed n -> (adj, (a, n))
          Constraint_Min n -> (max 0 (adj-1), (a, n + szStretch + signum adj))) adjustment constraints
    isMin (Constraint_Min _) = True
    isMin _ = False

-- | Produce a 'Region' given a starting orientation and region, and the offset
-- and main-axis size of the chunk.
chunk :: Orientation -> Region -> (Int, Int) -> Region
chunk o r (offset, sz) = case o of
  Orientation_Column -> r
    { _region_top = _region_top r + offset
    , _region_height = sz
    }
  Orientation_Row -> r
    { _region_left = _region_left r + offset
    , _region_width = sz
    }

-- ** The layout monad

-- | A class of operations for creating screen layouts.
class Monad m => MonadLayout t m | m -> t where
  -- | Creates a parent element in the current layout with the given size
  -- constraint, which lays out its children according to the provided
  -- orientation.
  axis :: Dynamic t Orientation -> Dynamic t Constraint -> m a -> m a
  -- | Creates a child element in the current layout with the given size
  -- constraint, returning the 'Region' that the child element is allocated.
  region :: Dynamic t Constraint -> m (Dynamic t Region)
  -- | Returns the orientation of the containing 'axis'.
  askOrientation :: m (Dynamic t Orientation)

-- | A monad transformer that collects layout constraints and provides a layout
-- solution that satisfies those constraints.
newtype Layout t m a = Layout
  { unLayout :: DynamicWriterT t (LayoutForest (Constraint, Orientation))
      (ReaderT (Dynamic t (LayoutTree (Region, Orientation))) m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadHold t
    , MonadSample t
    , MonadFix
    , TriggerEvent t
    , PerformEvent t
    , NotReady t
    , MonadReflexCreateTrigger t
    , HasDisplaySize t
    , PostBuild t
    , MonadNodeId
    , MonadIO
    )

instance MonadTrans (Layout t) where
  lift = Layout . lift . lift

instance MFunctor (Layout t) where
  hoist f = Layout . hoist (hoist f) . unLayout

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (Layout t m) where
  runWithReplace (Layout a) e = Layout $ runWithReplace a $ fmap unLayout e
  traverseIntMapWithKeyWithAdjust f m e = Layout $ traverseIntMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = Layout $ traverseDMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = Layout $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unLayout $ f k v) m e

instance (HasVtyWidgetCtx t m, HasDisplaySize t m, Reflex t, MonadFix m) => HasVtyWidgetCtx t (Layout t m) where
  localCtx f x = do
    solution <- Layout ask
    let orientation = snd . rootLT <$> solution
    lift $ localCtx f $ do
      dw <- displayWidth
      dh <- displayHeight
      let reg = Region 0 0 <$> dw <*> dh
      runLayout orientation reg x

-- | Apply a transformation to the context of a child 'Layout' action and run
-- that action
hoistRunLayout
  :: (HasDisplaySize t m, MonadFix m, Monad n)
  => (m a -> n b)
  -> Layout t m a
  -> Layout t n b
hoistRunLayout f x = do
  solution <- Layout ask
  let orientation = snd . rootLT <$> solution
  lift $ f $ do
    dw <- displayWidth
    dh <- displayHeight
    let reg = Region 0 0 <$> dw <*> dh
    runLayout orientation reg x

instance (HasVtyInput t m, HasDisplaySize t m, MonadFix m, Reflex t) => HasVtyInput t (Layout t m) where
  localInput = hoistRunLayout . localInput

instance (HasDisplaySize t m, ImageWriter t m, MonadFix m) => ImageWriter t (Layout t m) where
  mapImages f = hoistRunLayout (mapImages f)

instance (HasFocus t m, Monad m) => HasFocus t (Layout t m)

instance (Monad m, MonadNodeId m, Reflex t, MonadFix m) => MonadLayout t (Layout t m) where
  axis o c (Layout x) = Layout $ do
    nodeId <- getNextNodeId
    (result, forest) <- lift $ local (\t -> fromMaybe (LayoutTree (Region 0 0 0 0, Orientation_Column) mempty) . lookupLF nodeId . childrenLT <$> t) $ runDynamicWriterT x
    tellDyn $ singletonLF nodeId <$> (LayoutTree <$> ((,) <$> c <*> o) <*> forest)
    pure result
  region c = do
    nodeId <- lift getNextNodeId
    Layout $ tellDyn $ ffor c $ \c' -> singletonLF nodeId $ LayoutTree (c', Orientation_Row) mempty
    solutions <- Layout ask
    pure $ maybe (Region 0 0 0 0) (fst . rootLT) . lookupLF nodeId . childrenLT <$> solutions
  askOrientation = Layout $ asks $ fmap (snd . rootLT)

instance (MonadFix m, MonadFocus t m) => MonadFocus t (Layout t m) where
  makeFocus = lift makeFocus
  requestFocus = lift . requestFocus
  isFocused = lift . isFocused
  focusedId = lift focusedId
  subFoci (Layout x) = Layout $ do
    y <- ask
    ((a, w), sf) <- lift $ lift $ subFoci $ flip runReaderT y $ runDynamicWriterT x
    tellDyn w
    pure (a, sf)

-- | Runs a 'Layout' action, using the given orientation and region to
-- calculate layout solutions.
runLayout
  :: (MonadFix m, Reflex t)
  => Dynamic t Orientation
  -> Dynamic t Region
  -> Layout t m a
  -> m a
runLayout o r (Layout x) = do
  rec (result, w) <- runReaderT (runDynamicWriterT x) solutions
      let solutions = solve <$> o <*> r <*> w
  return result

-- | Initialize and run the layout monad, using all of the available screen space.
initLayout :: (HasDisplaySize t m, MonadFix m) => Layout t m a -> m a
initLayout f = do
  dw <- displayWidth
  dh <- displayHeight
  let r = Region 0 0 <$> dw <*> dh
  runLayout (pure Orientation_Column) r f

-- * The tile "window manager"
--
-- $tiling
-- Generally MonadLayout and MonadFocus are used together to build a user
-- interface. These functions check the available screen size and initialize
-- the layout monad with that information, and also initialize the focus monad.

-- | Initialize a 'Layout' and 'Focus'  management context, returning the produced 'FocusSet'.
initManager
  :: (HasDisplaySize t m, Reflex t, MonadHold t m, MonadFix m)
  => Layout t (Focus t m) a
  -> m (a, Dynamic t FocusSet)
initManager =
  runFocus . initLayout

-- | Initialize a 'Layout' and 'Focus'  management context.
initManager_
  :: (HasDisplaySize t m, Reflex t, MonadHold t m, MonadFix m)
  => Layout t (Focus t m) a
  -> m a
initManager_ = fmap fst . initManager

-- ** Layout tiles

-- *** Focusable

-- | A widget that is focusable and occupies a layout region based on the
-- provided constraint. Returns the 'FocusId' allowing for manual focus
-- management.
tile'
  :: (MonadNodeId m, MonadFix m, Reflex t, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m, ImageWriter t m)
  => Dynamic t Constraint
  -> m a
  -> m (FocusId, a)
tile' c w = do
  fid <- makeFocus
  r <- region c
  parentFocused <- isFocused fid
  rec (click, result, childFocused) <- pane r focused $ anyChildFocused $ \childFoc -> do
        m <- mouseDown V.BLeft
        x <- w
        pure (m, x, childFoc)
      let focused = (||) <$> parentFocused <*> childFocused
  requestFocus $ Refocus_Id fid <$ click
  pure (fid, result)

-- | A widget that is focusable and occupies a layout region based on the
-- provided constraint.
tile
  :: (MonadNodeId m, MonadFix m, Reflex t, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m, ImageWriter t m)
  => Dynamic t Constraint
  -> m a
  -> m a
tile c = fmap snd . tile' c

-- *** Unfocusable

-- | A widget that is not focusable and occupies a layout region based on the
-- provided constraint.
grout
  :: (Reflex t, MonadNodeId m, HasVtyWidgetCtx t m, MonadLayout t m, HasVtyInput t m, ImageWriter t m)
  => Dynamic t Constraint
  -> m a
  -> m a
grout c w = do
  r <- region c
  pane r (pure True) w
