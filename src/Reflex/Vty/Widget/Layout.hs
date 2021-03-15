{-|
Module: Reflex.Vty.Widget.Layout
Description: Monad transformer and tools for arranging widgets and building screen layouts
-}
{-# Language UndecidableInstances #-}

module Reflex.Vty.Widget.Layout where

import Control.Monad.NodeId (MonadNodeId(..), NodeId)
import Control.Monad.Reader
import Data.List
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
      (ReaderT (Demux t (Maybe FocusId))
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

newtype FocusId = FocusId NodeId
  deriving (Eq, Ord)

class (Monad m, Reflex t) => MonadFocus t m | m -> t where
  focusId :: m FocusId
  requestFocus :: Event t Refocus -> m ()
  isFocused :: FocusId -> m (Dynamic t Bool)

instance (Reflex t, Monad m, MonadNodeId m) => MonadFocus t (Focus t m) where
  focusId = do
    fid <- FocusId <$> lift getNextNodeId
    Focus $ tellDyn $ pure $ singletonFS fid
    pure fid
  requestFocus = Focus . tellEvent . fmap First
  isFocused fid = do
    sel <- Focus ask
    pure $ demuxed sel $ Just fid

runFocus
  :: (MonadFix m, MonadHold t m, Reflex t)
  => Focus t m a
  -> m (a, Dynamic t FocusSet)
runFocus (Focus x) = do
  rec ((a, focusIds), focusRequests) <- runEventWriterT $ flip runReaderT (demux sel) $ runDynamicWriterT x
      sel <- foldDyn f Nothing $ attach (current focusIds) focusRequests
  pure (a, focusIds)
  where
    f :: (FocusSet, First Refocus) -> Maybe FocusId -> Maybe FocusId
    f (fs, rf) mf = case getFirst rf of
      Refocus_Clear -> Nothing
      Refocus_Id fid -> Just fid
      Refocus_Shift n -> shiftFS fs mf n

-- TODO unify this with tile, perhaps
tileC
  :: (MonadNodeId m, MonadFix m, Reflex t)
  => Dynamic t Constraint
  -> VtyWidget t m a
  -> Layout t (Focus t (VtyWidget t m)) (FocusId, a)
tileC c w = do
  fid <- focusId
  focused <- isFocused fid
  r <- leaf c
  (click, result) <- lift $ lift $ pane r focused $ do
    click <- mouseDown V.BLeft
    result <- w
    pure (click, result)
  requestFocus $ Refocus_Id fid <$ click
  pure (fid, result)

tile
  :: (MonadNodeId m, MonadFix m, Reflex t)
  => VtyWidget t m a
  -> Layout t (Focus t (VtyWidget t m)) (FocusId, a)
tile = tileC (pure $ Constraint_Min 0)

tile_
  :: (MonadNodeId m, MonadFix m, Reflex t)
  => VtyWidget t m a
  -> Layout t (Focus t (VtyWidget t m)) a
tile_ = fmap snd . tile

-- * Layout

data LTree a = LTree a (LForest a)

newtype LForest a = LForest { unLForest :: OMap NodeId (LTree a) }

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

instance (HasVtyInput t m, Monad m) => HasVtyInput t (Layout t m)

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
    pure $ maybe (Region 0 0 0 0) (fst . ltreeRoot) . lookupLF nodeId . ltreeForest <$> solutions
  askOrientation = Layout $ asks $ fmap (snd . ltreeRoot)

instance MonadFocus t m => MonadFocus t (Layout t m) where
  focusId = lift focusId
  requestFocus = lift . requestFocus
  isFocused = lift . isFocused

-- | Datatype representing constraints on a widget's size along the main axis (see 'Orientation')
data Constraint = Constraint_Fixed Int
                | Constraint_Min Int
  deriving (Show, Read, Eq, Ord)

-- | The main-axis orientation of a 'Layout' widget
data Orientation = Orientation_Column
                 | Orientation_Row
  deriving (Show, Read, Eq, Ord)


row, col
  :: (Reflex t, MonadLayout t m)
  => m a
  -> m a
row = axis (pure Orientation_Row) (pure $ Constraint_Min 0)
col = axis (pure Orientation_Column) (pure $ Constraint_Min 0)

-- TODO possibly get rid of this and stretch
fixed
  :: (Reflex t, MonadLayout t m)
  => Dynamic t Int
  -> m (Dynamic t Region)
fixed = leaf . fmap Constraint_Fixed

stretch
  :: (Reflex t, MonadLayout t m)
  => Dynamic t Int
  -> m (Dynamic t Region)
stretch minSz = leaf (Constraint_Min <$> minSz)

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
      szStretch = floor $ leftover % (max numStretch 1)
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
  dw <- displayWidth
  dh <- displayHeight
  let r = Region <$> 0 <*> 0 <*> dw <*> dh
      text' t = do
        f <- focus
        richText (RichTextConfig $ current $ (\x -> if x then V.withStyle V.defAttr V.bold else V.defAttr) <$> f) t
  rec (_, _) <- runFocus $ do
        tabNavigation
        runLayout (pure Orientation_Column) r $ do
          col $ do
            tile_ $ text' "asdf"
            tile_ $ text' "asdf"
            row $ do
              tile_ $ text' "xyz"
              tile_ $ text' "xyz"
              tile_ $ text' "xyz"
              tile_ $ text' "xyz"
            tile_ $ text' "asdf"
            tile_ $ text' "asdf"
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
