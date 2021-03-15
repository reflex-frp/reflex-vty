{-|
Module: Reflex.Vty.Widget.Layout
Description: Monad transformer and tools for arranging widgets and building screen layouts
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Vty.Widget.Layout
  ( Orientation(..)
  , Constraint(..)
  , Layout(..)
  , runLayout
  , TileConfig(..)
  , tile
  , fixed
  , stretch
  , col
  , row
  , tabNavigation
  , askOrientation
  , Focus(..)
  , runFocus
  , focusId
  , LForest(..)

  , test
  ) where

import Control.Monad.NodeId (MonadNodeId(..), NodeId)
import Control.Monad.Reader
import Data.Default (Default(..))
import Data.List
import Data.Ratio ((%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as V
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import qualified Graphics.Vty.Attributes as V
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet

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

newtype Focus t m a = Focus
  { unFocus :: DynamicWriterT t FocusSet (ReaderT (Demux t (Maybe FocusId)) m) a
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
  lift = Focus . lift . lift

newtype FocusId = FocusId NodeId
  deriving (Eq, Ord)

focusId :: (MonadNodeId m, Reflex t) => Focus t m FocusId
focusId = do
  fid <- FocusId <$> lift getNextNodeId
  Focus $ tellDyn $ pure $ singletonFS fid
  pure fid

runFocus
  :: (MonadFix m, MonadHold t m, Reflex t)
  => Event t (Maybe FocusId)
  -> Focus t m a
  -> m (a, Dynamic t FocusSet)
runFocus e (Focus x) = do
  rec (a, focusIds) <- flip runReaderT (demux sel) $ runDynamicWriterT x
      sel <- holdDyn Nothing e
  pure (a, focusIds)

isFocused
  :: (Reflex t, Monad m)
  => FocusId
  -> Focus t m (Dynamic t Bool)
isFocused fid = do
  sel <- Focus ask
  pure $ demuxed sel $ Just fid

-- | Configuration options for and constraints on 'tile'
data TileConfig t = TileConfig
  { _tileConfig_constraint :: Dynamic t Constraint
    -- ^ 'Constraint' on the tile's size
  , _tileConfig_focusable :: Dynamic t Bool
    -- ^ Whether the tile is focusable
  }

instance Reflex t => Default (TileConfig t) where
  def = TileConfig (pure $ Constraint_Min 0) (pure True)

tile
  :: (MonadNodeId m, MonadFix m, Reflex t)
  => VtyWidget t m a
  -> Layout t (Focus t (VtyWidget t m)) a
tile w = do
  fid <- lift focusId
  focused <- lift $ isFocused fid
  stretch $ \r -> lift $ pane r focused w

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

-- | Datatype representing constraints on a widget's size along the main axis (see 'Orientation')
data Constraint = Constraint_Fixed Int
                | Constraint_Min Int
  deriving (Show, Read, Eq, Ord)

-- | The main-axis orientation of a 'Layout' widget
data Orientation = Orientation_Column
                 | Orientation_Row
  deriving (Show, Read, Eq, Ord)

axis
  :: (Monad m, MonadNodeId m, Reflex t, MonadFix m)
  => Orientation
  -> Constraint
  -> Layout t m a
  -> Layout t m a
axis o c (Layout x) = Layout $ do
  nodeId <- getNextNodeId
  (result, forest) <- lift $ local (\t -> (\(Just x) -> x) . lookupLF nodeId . ltreeForest <$> t) $ runDynamicWriterT x
  tellDyn $ fmap (singletonLF nodeId . LTree (c, o)) forest
  pure result

row, col
  :: (Monad m, MonadNodeId m, Reflex t, MonadFix m)
  => Constraint
  -> Layout t m a
  -> Layout t m a
row = axis Orientation_Row
col = axis Orientation_Column

leaf
  :: (Monad m, MonadNodeId m, Reflex t, MonadFix m)
  => Dynamic t Constraint
  -> (Dynamic t Region -> m a)
  -> Layout t m a
leaf c f = do
  nodeId <- lift getNextNodeId
  Layout $ tellDyn $ ffor c $ \c' -> singletonLF nodeId $ LTree (c', Orientation_Row) mempty
  solutions <- Layout ask
  let r = maybe (Region 0 0 0 0) (fst . ltreeRoot) . lookupLF nodeId . ltreeForest <$> solutions -- TODO revisit this fromMaybe
  lift $ f r

fixed
  :: (Monad m, MonadNodeId m, Reflex t, MonadFix m)
  => Dynamic t Int
  -> (Dynamic t Region -> m a)
  -> Layout t m a
fixed = leaf . fmap Constraint_Fixed

stretch
  :: (Monad m, MonadNodeId m, Reflex t, MonadFix m)
  => (Dynamic t Region -> m a)
  -> Layout t m a
stretch = leaf (pure $ Constraint_Min 0)

-- | Retrieve the current orientation of a 'Layout'
askOrientation :: (Monad m, Reflex t) => Layout t m (Dynamic t Orientation)
askOrientation = Layout $ asks $ fmap (snd . ltreeRoot)

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

{-
-- | Captures the click event in a 'VtyWidget' context and returns it. Useful for
-- requesting focus when using 'tile'.
clickable
  :: (Reflex t, Monad m)
  => VtyWidget t m a
  -> VtyWidget t m (Event t (), a)
clickable child = do
  click <- mouseDown V.BLeft
  a <- child
  return (() <$ click, a)

-}

-- | Produces an 'Event' that navigates forward one tile when the Tab key is pressed
-- and backward one tile when Shift+Tab is pressed.
tabNavigation :: (Reflex t, Monad m) => VtyWidget t m (Event t Int)
tabNavigation = do
  fwd <- fmap (const 1) <$> key (V.KChar '\t')
  back <- fmap (const (-1)) <$> key V.KBackTab
  return $ leftmost [fwd, back]

test :: IO ()
test = mainWidget $ do
  inp <- input
  dw <- displayWidth
  dh <- displayHeight
  let r = Region <$> 0 <*> 0 <*> dw <*> dh
      text' t = do
        f <- focus
        richText (RichTextConfig $ current $ (\x -> if x then V.withStyle V.defAttr V.bold else V.defAttr) <$> f) t
  tab <- tabNavigation
  rec (_, focusSet) <- runFocus (updated sel) $ runLayout (pure Orientation_Column) r $ do -- TODO start focused
        col (Constraint_Min 0) $ do
          tile $ text' "asdf"
          tile $ text' "asdf"
          row (Constraint_Min 0) $ do
            tile $ text' "xyz"
            tile $ text' "xyz"
            tile $ text' "xyz"
            tile $ text' "xyz"
          tile $ text' "asdf"
          tile $ text' "asdf"
      sel <- holdDyn Nothing $ attachWith (\(fs, s) t -> shiftFS fs s t) (current $ (,) <$> focusSet <*> sel) tab
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

  -- TODO These functions shouldn't be so higher order
  -- TODO request focus
