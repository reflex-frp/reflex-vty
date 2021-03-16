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

newtype FocusSet = FocusSet { unFocusSet :: OSet FocusId }

instance Semigroup FocusSet where
  FocusSet a <> FocusSet b = FocusSet $ a OSet.|<> b

instance Monoid FocusSet where
  mempty = FocusSet OSet.empty

singletonFS :: FocusId -> FocusSet
singletonFS = FocusSet . OSet.singleton

shiftFS :: FocusSet -> Maybe FocusId -> Int -> Maybe FocusId
shiftFS (FocusSet s) fid n = case OSet.findIndex <$> fid <*> pure s of
  Nothing -> OSet.elemAt s 0
  Just Nothing -> OSet.elemAt s 0
  Just (Just ix) -> OSet.elemAt s $ mod (ix + n) (OSet.size s)

data Refocus = Refocus_Shift Int
             | Refocus_Id FocusId
             | Refocus_Clear

newtype Focus t m a = Focus
  { unFocus :: DynamicWriterT t FocusSet
      (ReaderT (Dynamic t (Maybe FocusId)) -- TODO Demux?
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

newtype FocusId = FocusId NodeId
  deriving (Eq, Ord)

class (Monad m, Reflex t) => MonadFocus t m | m -> t where
  focusId :: m FocusId
  requestFocus :: Event t Refocus -> m ()
  isFocused :: FocusId -> m (Dynamic t Bool)
  subFoci :: m a -> m (a, Dynamic t FocusSet)
  focusedId :: m (Dynamic t (Maybe FocusId))

instance (Reflex t, MonadFix m, MonadNodeId m) => MonadFocus t (Focus t m) where
  focusId = do
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

tile
  :: (MonadNodeId m, MonadFix m, Reflex t, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Constraint
  -> m a
  -> m (FocusId, a)
tile c w = do
  fid <- focusId
  r <- leaf c
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

tile_
  :: (MonadNodeId m, MonadFix m, Reflex t, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Constraint
  -> m a
  -> m a
tile_ c = fmap snd . tile c

-- * Layout

data LTree a = LTree a (LForest a)
  deriving (Show)

newtype LForest a = LForest { unLForest :: OMap NodeId (LTree a) }
  deriving (Show)

instance Semigroup (LForest a) where
  LForest a <> LForest b = LForest $ a OMap.|<> b

instance Monoid (LForest a) where
  mempty = LForest OMap.empty

lookupLF :: NodeId -> LForest a -> Maybe (LTree a)
lookupLF n (LForest a) = OMap.lookup n a

singletonLF :: NodeId -> LTree a -> LForest a
singletonLF n t = LForest $ OMap.singleton (n, t)

fromListLF :: [(NodeId, LTree a)] -> LForest a
fromListLF = LForest . OMap.fromList

ltreeRoot :: LTree a -> a
ltreeRoot (LTree a _) = a

ltreeForest :: LTree a -> LForest a
ltreeForest (LTree _ a) = a

newtype Layout t m a = Layout
  { unLayout :: DynamicWriterT t (LForest (Constraint, Orientation))
      (ReaderT (Dynamic t (LTree (Region, Orientation))) m) a
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

instance (HasVtyWidgetCtx t m, HasDisplaySize t m, Reflex t, MonadFix m) => HasVtyWidgetCtx t (Layout t m) where
  localCtx f g x = do
    solution <- Layout ask
    let orientation = snd . ltreeRoot <$> solution
    lift $ localCtx f g $ do
      dw <- displayWidth
      dh <- displayHeight
      let region = Region 0 0 <$> dw <*> dh
      runLayout orientation region x

instance (HasVtyInput t m, Monad m) => HasVtyInput t (Layout t m)
instance ImageWriter t m => ImageWriter t (Layout t m)
instance (HasFocus t m, Monad m) => HasFocus t (Layout t m)

-- TODO Naming: To Has or not to Has?
class Monad m => MonadLayout t m | m -> t where
  axis :: Dynamic t Orientation -> Dynamic t Constraint -> m a -> m a
  leaf :: Dynamic t Constraint -> m (Dynamic t Region)
  askOrientation :: m (Dynamic t Orientation)

instance (Monad m, MonadNodeId m, Reflex t, MonadFix m) => MonadLayout t (Layout t m) where
  axis o c (Layout x) = Layout $ do
    nodeId <- getNextNodeId
    (result, forest) <- lift $ local (\t -> (\(Just a) -> a) . lookupLF nodeId . ltreeForest <$> t) $ runDynamicWriterT x
    tellDyn $ singletonLF nodeId <$> (LTree <$> ((,) <$> c <*> o) <*> forest)
    pure result
  leaf c = do
    nodeId <- lift getNextNodeId
    Layout $ tellDyn $ ffor c $ \c' -> singletonLF nodeId $ LTree (c', Orientation_Row) mempty
    solutions <- Layout ask
    pure $ maybe (error "leaf: could not find layout solution") (fst . ltreeRoot) . lookupLF nodeId . ltreeForest <$> solutions
  askOrientation = Layout $ asks $ fmap (snd . ltreeRoot)

instance (MonadFix m, MonadFocus t m) => MonadFocus t (Layout t m) where
  focusId = lift focusId
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


row, col
  :: (Reflex t, MonadNodeId m, MonadFix m, MonadLayout t m)
  => m a
  -> m a
row = axis (pure Orientation_Row) (pure $ Constraint_Min 0)
col = axis (pure Orientation_Column) (pure $ Constraint_Min 0)

fixed
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Int
  -> m a
  -> m (FocusId, a)
fixed n = tile (Constraint_Fixed <$> n)

fixed_
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Int
  -> m a
  -> m a
fixed_ n = fmap snd . fixed n

stretch
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Int
  -> m a
  -> m (FocusId, a)
stretch minSz = tile (Constraint_Min <$> minSz)

stretch_
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => Dynamic t Int
  -> m a
  -> m a
stretch_ minSz = fmap snd . tile (Constraint_Min <$> minSz)

flex
  :: (Reflex t, MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => m a
  -> m (FocusId, a)
flex = tile (pure $ Constraint_Min 0)

flex_
  :: (Reflex t,MonadNodeId m, MonadFix m, HasVtyWidgetCtx t m, HasVtyInput t m, MonadFocus t m, MonadLayout t m)
  => m a
  -> m a
flex_ = fmap snd . flex

ltreeConstraint :: LTree (Constraint, a) -> Constraint
ltreeConstraint (LTree (c, _) _) = c

solve
  :: Orientation
  -> Region
  -> LForest (Constraint, Orientation)
  -> LTree (Region, Orientation)
solve o0 r0 (LForest cs) =
  let a = map (\(x, t) -> ((x, t), ltreeConstraint t)) $ OMap.assocs cs
      extent = case o0 of
        Orientation_Row -> _region_width r0
        Orientation_Column -> _region_height r0
      sizes = computeEdges $ computeSizes extent a
      chunks = [ (nodeId, solve o1 r1 f)
               | ((nodeId, LTree (_, o1) f), sz) <- sizes
               , let r1 = chunk o0 r0 sz
               ]
  in LTree (r0, o0) $ fromListLF chunks

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

-- | Compute the size of each widget "@k@" based on the total set of 'Constraint's
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
  where
    isMin (Constraint_Min _) = True
    isMin _ = False

computeEdges :: [(a, Int)] -> [(a, (Int, Int))]
computeEdges = ($ []) . fst . foldl (\(m, offset) (a, sz) ->
  (((a, (offset, sz)) :) . m, sz + offset)) (id, 0)

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
      fixed_ 5 $ box' $ row $ do
        flex_ $ do
          text' "asdf"
        flex_ $ text' "xyz"
      fixed_ 5 $ do
        text' "   1"
      row $ do
        tile_ (pure $ Constraint_Min 0) $ do
          text' "2"
        flex_ $ text' "3"
        flex_ $ text' "xyz"
        flex_ $ text' "xyz"
      flex_ $ text' "asdf"
      flex_ $ text' "asdf"
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

-- TODO: 'Manager'? as in "window manager"
initManager
  :: (HasDisplaySize t m, Reflex t, MonadHold t m, MonadFix m)
  => Layout t (Focus t m) a
  -> m (a, Dynamic t FocusSet)
initManager f = do
  dw <- displayWidth
  dh <- displayHeight
  let r = Region 0 0 <$> dw <*> dh
  runFocus $ runLayout (pure Orientation_Column) r f

initManager_
  :: (HasDisplaySize t m, Reflex t, MonadHold t m, MonadFix m)
  => Layout t (Focus t m) a
  -> m a
initManager_ = fmap fst . initManager
