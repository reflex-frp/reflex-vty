{-|
Module: Reflex.Vty.Widget.Layout
Description: Monad transformer and tools for arranging widgets and building screen layouts
-}
{-# Language UndecidableInstances #-}

module Reflex.Vty.Widget.Layout where

import Control.Applicative (liftA2)
import Control.Monad.NodeId (MonadNodeId(..), NodeId)
import Control.Monad.Reader
import Data.List (mapAccumL)
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import Data.Ratio ((%))
import Data.Semigroup (First(..))
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import qualified Graphics.Vty as V

import Reflex
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Vty.Widget

-- * Focus

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

-- | Given a 'FocusSet', a currently focused element, and a number of positions
-- to move by, determine the newly focused element.
shiftFS :: FocusSet -> Maybe FocusId -> Int -> Maybe FocusId
shiftFS (FocusSet s) fid n = case OSet.findIndex <$> fid <*> pure s of
  Nothing -> OSet.elemAt s 0
  Just Nothing -> OSet.elemAt s 0
  Just (Just ix) -> OSet.elemAt s $ mod (ix + n) (OSet.size s)

-- | Operations that change the currently focused element.
data Refocus = Refocus_Shift Int -- ^ Shift the focus by a certain number of positions (see 'shiftFS')
             | Refocus_Id FocusId -- ^ Focus a particular element
             | Refocus_Clear -- ^ Remove focus from all elements

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
    )

instance MonadTrans (Focus t) where
  lift = Focus . lift . lift . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (Focus t m) where
  runWithReplace (Focus a) e = Focus $ runWithReplace a $ fmap unFocus e
  traverseIntMapWithKeyWithAdjust f m e = Focus $ traverseIntMapWithKeyWithAdjust (\k v -> unFocus $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = Focus $ traverseDMapWithKeyWithAdjust (\k v -> unFocus $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = Focus $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unFocus $ f k v) m e

instance (HasVtyInput t m, Monad m) => HasVtyInput t (Focus t m)

instance (HasVtyWidgetCtx t m, Reflex t, MonadFix m) => HasVtyWidgetCtx t (Focus t m) where
  localCtx f g (Focus w) = Focus $ do
    d <- ask
    ((a, fs), e) <- lift $ lift $ lift $ localCtx f g $ runEventWriterT $ flip runReaderT d $ runDynamicWriterT w
    tellEvent e
    tellDyn fs
    return a

instance ImageWriter t m => ImageWriter t (Focus t m)

instance (HasFocus t m, Monad m) => HasFocus t (Focus t m)

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
      Refocus_Shift n -> shiftFS fs mf n

-- | A widget that is focusable and occupies a layout region based on the
-- provided constraint. Returns the 'FocusId' allowing for manual focus
-- management.
tile'
  :: (MonadNodeId m, MonadFix m, Reflex t, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Constraint
  -> m a
  -> m (FocusId, a)
tile' c w = do
  fid <- makeFocus
  r <- region c
  fid' <- focusedId
  parentFocused <- isFocused fid
  rec (click, result, subs) <- pane r focused $ do
        m <- mouseDown V.BLeft
        (x, sf) <- subFoci w
        pure (m, x, sf)
      let childFocused = liftA2 (\f s -> case f of
            Nothing -> False
            Just f' -> OSet.member f' $ unFocusSet s) fid' subs
      let focused = (||) <$> parentFocused <*> childFocused
  requestFocus $ Refocus_Id fid <$ click
  pure (fid, result)

-- | A widget that is focusable and occupies a layout region based on the
-- provided constraint.
tile
  :: (MonadNodeId m, MonadFix m, Reflex t, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Constraint
  -> m a
  -> m a
tile c = fmap snd . tile' c

-- | A widget that is not focusable and occupies a layout region based on the
-- provided constraint.
unfocusable
  :: (Reflex t, MonadNodeId m, HasVtyWidgetCtx t m, MonadLayout t m)
  => Dynamic t Constraint
  -> m a
  -> m a
unfocusable c w = do
  r <- region c
  pane r (pure False) w

-- * Layout

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
    )

instance MonadTrans (Layout t) where
  lift = Layout . lift . lift

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (Layout t m) where
  runWithReplace (Layout a) e = Layout $ runWithReplace a $ fmap unLayout e
  traverseIntMapWithKeyWithAdjust f m e = Layout $ traverseIntMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = Layout $ traverseDMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = Layout $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unLayout $ f k v) m e


instance (HasVtyWidgetCtx t m, HasDisplaySize t m, Reflex t, MonadFix m) => HasVtyWidgetCtx t (Layout t m) where
  localCtx f g x = do
    solution <- Layout ask
    let orientation = snd . rootLT <$> solution
    lift $ localCtx f g $ do
      dw <- displayWidth
      dh <- displayHeight
      let reg = Region 0 0 <$> dw <*> dh
      runLayout orientation reg x

instance (HasVtyInput t m, Monad m) => HasVtyInput t (Layout t m)

instance ImageWriter t m => ImageWriter t (Layout t m)

instance (HasFocus t m, Monad m) => HasFocus t (Layout t m)

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

instance (Monad m, MonadNodeId m, Reflex t, MonadFix m) => MonadLayout t (Layout t m) where
  axis o c (Layout x) = Layout $ do
    nodeId <- getNextNodeId
    (result, forest) <- lift $ local (\t -> (\(Just a) -> a) . lookupLF nodeId . childrenLT <$> t) $ runDynamicWriterT x
    tellDyn $ singletonLF nodeId <$> (LayoutTree <$> ((,) <$> c <*> o) <*> forest)
    pure result
  region c = do
    nodeId <- lift getNextNodeId
    Layout $ tellDyn $ ffor c $ \c' -> singletonLF nodeId $ LayoutTree (c', Orientation_Row) mempty
    solutions <- Layout ask
    pure $ maybe (error "region: could not find layout solution") (fst . rootLT) . lookupLF nodeId . childrenLT <$> solutions
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

-- | Datatype representing constraints on a widget's size along the main axis (see 'Orientation')
data Constraint = Constraint_Fixed Int
                | Constraint_Min Int
  deriving (Show, Read, Eq, Ord)

-- | The main-axis orientation of a 'Layout' widget
data Orientation = Orientation_Column
                 | Orientation_Row
  deriving (Show, Read, Eq, Ord)

-- | Create a row-oriented 'axis'
row
  :: (Reflex t, MonadNodeId m, MonadFix m, MonadLayout t m)
  => m a
  -> m a
row = axis (pure Orientation_Row) (pure $ Constraint_Min 0)

-- | Create a column-oriented 'axis'
col
  :: (Reflex t, MonadNodeId m, MonadFix m, MonadLayout t m)
  => m a
  -> m a
col = axis (pure Orientation_Column) (pure $ Constraint_Min 0)

-- | A tile constrained to the given size. Returns the 'FocusId' of the tile,
-- allowing for manual focus management.
fixed'
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Int
  -> m a
  -> m (FocusId, a)
fixed' n = tile' (Constraint_Fixed <$> n)

-- | A tile constrained to the given size.
fixed
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Int
  -> m a
  -> m a
fixed n = fmap snd . fixed' n

-- | A tile that can grow, but will always occupy at least the given minimum
-- size. Returns the 'FocusId' of the tile, allowing for manual focus
-- management.
stretch'
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Int
  -> m a
  -> m (FocusId, a)
stretch' minSz = tile' (Constraint_Min <$> minSz)

-- | A tile that can grow, but will always occupy at least the given minimum
-- size.
stretch
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Int
  -> m a
  -> m a
stretch minSz = fmap snd . stretch' minSz

-- | A tile that can grow or shrink without constraint. Returns the 'FocusId'
-- of the tile, allowing for manual focus management.
flex'
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => m a
  -> m (FocusId, a)
flex' = tile' (pure $ Constraint_Min 0)

-- | A tile that can grow or shrink without constraint.
flex
  :: (Reflex t,MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => m a
  -> m a
flex = fmap snd . flex'

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
      let minTotal = sum $ ffor constraints $ \case
            (_, Constraint_Fixed n) -> n
            (_, Constraint_Min n) -> n
          leftover = max 0 (available - minTotal)
          numStretch = length $ filter (isMin . snd) constraints
          szStretch = floor $ leftover % max numStretch 1
          adjustment = max 0 $ available - minTotal - szStretch * numStretch
      in snd $ mapAccumL (\adj (a, c) -> case c of
          Constraint_Fixed n -> (adj, (a, n))
          Constraint_Min n -> (0, (a, n + szStretch + adj))) adjustment constraints
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

-- | Request focus be shifted backward and forward based on tab presses. <Tab>
-- shifts focus forward and <Shift+Tab> shifts focus backward.
tabNavigation :: (Reflex t, MonadNodeId m, HasVtyInput t m, MonadFocus t m) => m ()
tabNavigation = do
  fwd <- fmap (const 1) <$> key (V.KChar '\t')
  back <- fmap (const (-1)) <$> key V.KBackTab
  requestFocus $ Refocus_Shift <$> leftmost [fwd, back]

test :: IO ()
test = mainWidget $ do
  inp <- input
  let box' w = do
        f <- focus
        box (ffor (current f) $ \x -> if x then doubleBoxStyle else singleBoxStyle) $ w
      text' t = do
        f <- focus
        box' $ richText (RichTextConfig $ current $ (\x -> if x then V.withStyle V.defAttr V.underline else V.defAttr) <$> f) t
  initManager_ $ do
    tabNavigation
    col $ do
      fixed 5 $ box' $ row $ do
        flex $ do
          text' "asdf"
        flex $ text' "xyz"
      fixed 5 $ do
        text' "   1"
      row $ do
        flex $ do
          text' "2"
        flex $ text' "3"
        flex $ text' "xyz"
        flex $ text' "xyz"
      flex $ text' "asdf"
      flex $ text' "asdf"
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

-- | Initialize a 'Layout' and 'Focus'  management context, returning the produced 'FocusSet'.
initManager
  :: (HasDisplaySize t m, Reflex t, MonadHold t m, MonadFix m)
  => Layout t (Focus t m) a
  -> m (a, Dynamic t FocusSet)
initManager f = do
  dw <- displayWidth
  dh <- displayHeight
  let r = Region 0 0 <$> dw <*> dh
  runFocus $ runLayout (pure Orientation_Column) r f

-- | Initialize a 'Layout' and 'Focus'  management context.
initManager_
  :: (HasDisplaySize t m, Reflex t, MonadHold t m, MonadFix m)
  => Layout t (Focus t m) a
  -> m a
initManager_ = fmap fst . initManager
