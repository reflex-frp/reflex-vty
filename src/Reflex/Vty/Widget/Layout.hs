{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE UndecidableInstances       #-}


module Potato.Reflex.Vty.Widget.Layout2
  (  Orientation(..)
  , Constraint(..)
  , Layout
  , runLayout
  , TileConfig(..)
  , tile
  , fixed
  , fixedD
  , stretch
  , stretchD
  , col
  , row
  , dummy
  , beginLayout
  , beginLayoutD
  , tabNavigation
  , askOrientation
  ) where

import           Prelude

import qualified Relude                 as R

import           Control.Monad.Identity (Identity (..))
import           Control.Monad.NodeId   (MonadNodeId (..), NodeId (..))
import           Control.Monad.Reader
import           Data.Bimap             (Bimap)
import qualified Data.Bimap             as Bimap
import           Data.Default           (Default (..))
import           Data.Dependent.Map     (DMap, DSum ((:=>)))
import qualified Data.Dependent.Map     as DMap
import           Data.Functor.Misc
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified Data.Map.Internal      as Map (Map (Bin, Tip))
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Monoid            hiding (First (..))
import           Data.Ratio             ((%))
import           Data.Semigroup         (First (..))
import           Data.Traversable       (mapAccumL)
import           Data.Tuple.Extra
import qualified Graphics.Vty           as V
import           Unsafe.Coerce

import           Reflex
import           Reflex.Host.Class      (MonadReflexCreateTrigger)
import           Reflex.Vty.Widget

import           Control.Exception      (assert)

-- | The main-axis orientation of a 'Layout' widget
data Orientation = Orientation_Column
                 | Orientation_Row
  deriving (Show, Read, Eq, Ord)

data LayoutSegment = LayoutSegment
  { _layoutSegment_offset :: Int
  , _layoutSegment_size   :: Int
  }

data LayoutCtx t = LayoutCtx
  { _layoutCtx_regions            :: Dynamic t (Map NodeId LayoutSegment)
  , _layoutCtx_focusSelfDemux     :: Demux t (Maybe NodeId)
  , _layoutCtx_orientation        :: Dynamic t Orientation
  , _layoutCtx_focusChildSelector :: EventSelector t (Const2 NodeId (Maybe Int))
  }


-- | The Layout monad transformer keeps track of the configuration (e.g., 'Orientation') and
-- 'Constraint's of its child widgets, apportions vty real estate to each, and acts as a
-- switchboard for focus requests. See 'tile' and 'runLayout'.
newtype Layout t m a = Layout
  { unLayout :: EventWriterT t (First (NodeId, Int))
      (DynamicWriterT t (Endo [(NodeId, (Bool, Constraint), LayoutDebugTree t, Int)])
        (ReaderT (LayoutCtx t)
          (VtyWidget t m))) a
  } deriving
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
    , MonadNodeId
    , PostBuild t
    )

instance MonadTrans (Layout t) where
  lift x = Layout $ lift $ lift $ lift $ lift x

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (Layout t m) where
  runWithReplace (Layout a) e = Layout $ runWithReplace a $ fmap unLayout e
  traverseIntMapWithKeyWithAdjust f m e = Layout $ traverseIntMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = Layout $ traverseDMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = Layout $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unLayout $ f k v) m e


findNearestFloor_ :: (Ord k) => k -> (k, a) -> (k, a) -> Map k a -> Maybe (k, a)
findNearestFloor_ target leftmost parent Map.Tip = if target < fst leftmost
  then Nothing -- error $ "Map.findNearestFloorSure: map has no element <= " <> show target
  else if target < fst parent
    then Just leftmost
    else Just parent
findNearestFloor_ target leftmost _ (Map.Bin _ k a l r) = if target == k
  then Just (k, a)
  else if target < k
    then findNearestFloor_ target leftmost (k, a) l
    else findNearestFloor_ target (k, a) (k, a) r

findNearestFloor :: (Ord k) => k -> Map k a -> Maybe (k,a)
findNearestFloor _ Map.Tip = Nothing
-- TODO I don't think we need to do findMin here, just pass in (k,x) as a placeholder value
findNearestFloor target m@(Map.Bin _ k x _ _) = findNearestFloor_ target (Map.findMin m) (k, x) m



fanFocusEv :: (Reflex t) => Behavior t (Maybe (NodeId, Int)) -> Event t (Maybe (NodeId, Int)) -> EventSelector t (Const2 NodeId (Maybe Int))
fanFocusEv focussed focusReqIx = fan $ attachWith attachfn focussed focusReqIx where
  attachfn mkv0 mkv1 = case mkv1 of
    Nothing -> case mkv0 of
      Nothing      -> DMap.empty
      Just (k0,v0) -> DMap.fromList [Const2 k0 :=> Identity Nothing]
    Just (k1,v1) -> case mkv0 of
      Nothing -> DMap.fromList [Const2 k1 :=> Identity (Just v1)]
      Just (k0,v0) | k0 == k1 && v0 == v1 -> DMap.empty
      Just (k0,v0) | k0 == k1 -> DMap.fromList [Const2 k1 :=> Identity (Just v1)]
      Just (k0,v0) -> DMap.fromList [Const2 k0 :=> Identity Nothing,
                          Const2 k1 :=> Identity (Just v1)]

-- | Run a 'Layout' action
runLayoutD
  :: forall t m a. (Reflex t, MonadFix m, MonadHold t m, Monad m, MonadNodeId m)
  => Dynamic t Orientation -- ^ The main-axis 'Orientation' of this 'Layout'
  -> Maybe Int -- ^ The positional index of the initially focused tile
  -> Layout t m a -- ^ The 'Layout' widget
  -> LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
runLayoutD ddir mfocus0 (Layout child) = LayoutVtyWidget . ReaderT $ \focusReqIx -> mdo
  dw <- displayWidth
  dh <- displayHeight
  let main = ffor3 ddir dw dh $ \d w h -> case d of
        Orientation_Column -> h
        Orientation_Row    -> w
  ((a, focusReq), queriesEndo) <- runReaderT (runDynamicWriterT $ runEventWriterT child) $ LayoutCtx solutionMap focusDemux ddir focusChildSelector
  let queries = flip appEndo [] <$> queriesEndo
      solution = ffor2 main queries $ \sz qs -> Map.fromList
        . Map.elems
        . computeEdges
        . computeSizes sz
        . fmap (\(nodeid,(_,constraint),_,_) -> (nodeid,constraint))
        . Map.fromList
        . zip [0::Integer ..]
        $ qs
      solutionMap = ffor solution $ \ss -> ffor ss $ \(offset, sz) -> LayoutSegment
        { _layoutSegment_offset = offset
        , _layoutSegment_size = sz
        }


      focusableMapAccumFn acc (nodeId, (_, _), _, nKiddos) = (nextAcc, value) where
        nextAcc = acc + nKiddos
        value = (acc, nodeId)
      focusable' = fmap (Bimap.fromList . snd . mapAccumL focusableMapAccumFn 0) $
        ffor queries $ \qs -> fforMaybe qs $ \n@(_, (f, _), _, nc) ->
          if f && nc > 0 then Just n else Nothing
      focusable = focusable'

      -- ix is focus in self index space
      -- fst of return value is child node id to focus
      -- snd of return value is focus in child's index space
      findChildFocus :: Bimap Int NodeId -> Int -> Maybe (NodeId, Int)
      findChildFocus fm ix = findNearestFloor ix (Bimap.toMap fm) >>= \(ixl, t) -> Just (t, ix-ixl)

      adjustFocus
        :: (Bimap Int NodeId)
        -> Either Int (NodeId, Int) -- left is self index, right is (child id, child index)
        -> Maybe (Int, (NodeId, Int)) -- fst is self index, snd is (child id, child index)
      adjustFocus fm (Left ix) = do
        x <- findChildFocus fm ix
        return (ix, x)
      adjustFocus fm (Right (goto, ixrel)) = do
        ix <- Bimap.lookupR goto fm
        return (ix+ixrel, (goto, ixrel))

      focusChange = attachWith adjustFocus (current focusable)
        -- TODO handle Nothing case in both places (so that event produces Nothing in this case)
        $ leftmost [ fmap Right . fmap getFirst $ focusReq, Left <$> (fmapMaybe id focusReqIx)]


  fm0 <- sample . current $ focusable
  totalKiddos <- sample . current $ fmap (sum . fmap (\(_,_,_,k) -> k)) queries

  -- brief explanation of overly complicated focus tracking
  -- focus is propogated in 2 ways
  -- focusReq (focus from bottom up)
  -- focusReqIx (focus from top down)
  -- focussed tracks the focus state
  -- focusDemux is used to pass the 'pane's of immediate children

  -- fst is index we want to focus in self index space, snd is node id we want to focus
  focussed :: Dynamic t (Maybe (Int, (NodeId, Int))) <- holdDyn initialFocus (focusChange)
  let
    initialFocus :: Maybe (Int, (NodeId, Int)) = do
      f0 <- mfocus0
      cf0 <- findChildFocus fm0 f0
      return (f0, cf0)

    focussedForDemux = fmap (fmap (fst . snd)) focussed
    focusDemux :: Demux t (Maybe NodeId) = demux focussedForDemux

    focusReqWithNodeId :: Event t (Maybe (NodeId, Int))
    focusReqWithNodeId = attachWith (\fm mix -> mix >>= \ix -> findChildFocus fm ix) (current focusable) (focusReqIx)
    focusChildSelector = fanFocusEv (current $ fmap (fmap snd) focussed) (focusReqWithNodeId)

  return (emptyLayoutDebugTree, (fmap (fmap fst)) focussed, totalKiddos, a)

-- | Run a 'Layout' action
runLayout
  :: (MonadFix m, MonadHold t m, PostBuild t m, Monad m, MonadNodeId m)
  => Dynamic t Orientation -- ^ The main-axis 'Orientation' of this 'Layout'
  -> Maybe Int -- ^ The positional index of the initially focused tile
  -> Layout t m a -- ^ The 'Layout' widget
  -> LayoutVtyWidget t m a
runLayout ddir mfocus0 layout = fmap (\(_,_,_,a)->a) $ runLayoutD ddir mfocus0 layout

-- | Tiles are the basic building blocks of 'Layout' widgets. Each tile has a constraint
-- on its size and ability to grow and on whether it can be focused. It also allows its child
-- widget to request focus.
tile
  :: forall t b widget x m a. (Reflex t, IsLayoutReturn t b a, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => TileConfig t -- ^ The tile's configuration
  -> widget t m (Event t x, b) -- ^ A child widget. The 'Event' that it returns is used to request that it be focused.
  -> Layout t m a
tile (TileConfig con focusable) child = mdo
  nodeId <- getNextNodeId
  -- by calling getLayoutTree/getLayoutNumChildren here, we store the children's layout info inside the DynamicWriter
  -- runLayoutD will extract this info later
  Layout $ tellDyn $ ffor2 con focusable $ \c f -> Endo ((nodeId, (f, c), getLayoutTree @t @b @a b, nKiddos):)
  seg <- Layout $ asks $
    fmap (Map.findWithDefault (LayoutSegment 0 0) nodeId) . _layoutCtx_regions
  dw <- displayWidth
  dh <- displayHeight
  o <- askOrientation
  let cross = join $ ffor o $ \case
        Orientation_Column -> dw
        Orientation_Row -> dh
  let reg = DynRegion
        { _dynRegion_top = ffor2 seg o $ \s -> \case
            Orientation_Column -> _layoutSegment_offset s
            Orientation_Row -> 0
        , _dynRegion_left = ffor2 seg o $ \s -> \case
            Orientation_Column -> 0
            Orientation_Row -> _layoutSegment_offset s
        , _dynRegion_width = ffor3 seg cross o $ \s c -> \case
            Orientation_Column -> c
            Orientation_Row -> _layoutSegment_size s
        , _dynRegion_height = ffor3 seg cross o $ \s c -> \case
            Orientation_Column -> _layoutSegment_size s
            Orientation_Row -> c
        }
  let nKiddos = getLayoutNumChildren @t @b @a b

  focusChildSelector <- Layout $ asks _layoutCtx_focusChildSelector
  let focusChildEv = select focusChildSelector (Const2 nodeId)
  focussed <- Layout $ asks _layoutCtx_focusSelfDemux
  (focusReq, b) <- Layout $ lift $ lift $ lift $
    pane reg (demuxed focussed $ Just nodeId) $ runIsLayoutVtyWidget child (focusChildEv)
  Layout $ tellEvent $ if nKiddos > 0
    then fmap (First . swap) $ attachPromptlyDyn (fmap (fromMaybe 0) $ getLayoutFocussedDyn @t @b @a b) (nodeId <$ focusReq)
    else never
  return $ getLayoutResult @t b


-- | Configuration options for and constraints on 'tile'
data TileConfig t = TileConfig
  { _tileConfig_constraint :: Dynamic t Constraint
    -- ^ 'Constraint' on the tile's size
  , _tileConfig_focusable  :: Dynamic t Bool
    -- ^ Whether the tile is focusable
  }


instance Reflex t => Default (TileConfig t) where
  def = TileConfig (pure $ Constraint_Min 0) (pure True)

-- | A 'tile' of a fixed size that is focusable and gains focus on click
fixed'
  :: (Reflex t, IsLayoutReturn t b a, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> widget t m b
  -> Layout t m a
fixed' sz = tile (def { _tileConfig_constraint =  Constraint_Fixed <$> sz }) . clickable

fixedD
  :: (Reflex t, Monad m, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
  -> Layout t m a
fixedD = fixed'

fixed
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> widget t m a
  -> Layout t m a
fixed = fixed'

-- | A 'tile' that can stretch (i.e., has no fixed size) and has a minimum size of 0.
-- This tile is focusable and gains focus on click.
stretch'
  :: (Reflex t, IsLayoutReturn t b a, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => widget t m b
  -> Layout t m a
stretch' = tile def . clickable

stretchD
  :: (Reflex t, Monad m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
  -> Layout t m a
stretchD = stretch'

stretch
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => widget t m a
  -> Layout t m a
stretch = stretch'

-- | A version of 'runLayout' that arranges tiles in a column and uses 'tabNavigation' to
-- change tile focus.
col
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => Layout t m a
  -> LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
col child = runLayoutD (pure Orientation_Column) (Just 0) child

-- | A version of 'runLayout' that arranges tiles in a row and uses 'tabNavigation' to
-- change tile focus.
row
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => Layout t m a
  -> LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
row child = runLayoutD (pure Orientation_Row) (Just 0) child

-- | Testing placeholder DELETE
dummy :: (Reflex t, Monad m) => LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, ())
dummy = return (emptyLayoutDebugTree, constDyn Nothing, 0, ())

-- | Produces an 'Event' that navigates forward one tile when the Tab key is pressed
-- and backward one tile when Shift+Tab is pressed.
tabNavigation :: (Reflex t, Monad m) => VtyWidget t m (Event t Int)
tabNavigation = do
  fwd <- fmap (const 1) <$> key (V.KChar '\t')
  back <- fmap (const (-1)) <$> key V.KBackTab
  return $ leftmost [fwd, back]

-- | Captures the click event in a 'VtyWidget' context and returns it. Useful for
-- requesting focus when using 'tile'.
clickable
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m)
  => widget t m a
  -> LayoutVtyWidget t m (Event t (), a)
clickable child = LayoutVtyWidget . ReaderT $ \focusEv -> do
  click <- mouseDown V.BLeft
  a <- runIsLayoutVtyWidget child focusEv
  return (() <$ click, a)

beginLayoutD ::
  forall m t a. (MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
  -> VtyWidget t m (LayoutDebugTree t, a)
beginLayoutD child = mdo
  -- TODO consider unfocusing if this loses focus
  --focussed <- focus
  tabEv <- tabNavigation
  let focusChildEv = fmap (\(mcur, shift) -> maybe (Just 0) (\cur -> Just $ (shift + cur) `mod` totalKiddos) mcur) (attach (current indexDyn) tabEv)
  (ldt, indexDyn, totalKiddos, a) <- runIsLayoutVtyWidget child focusChildEv
  return (ldt, a)

-- |
beginLayout ::
  forall m t b a. (MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
  -> VtyWidget t m a
beginLayout = fmap snd . beginLayoutD

-- | Retrieve the current orientation of a 'Layout'
askOrientation :: Monad m => Layout t m (Dynamic t Orientation)
askOrientation = Layout $ asks _layoutCtx_orientation

-- | Datatype representing constraints on a widget's size along the main axis (see 'Orientation')
data Constraint = Constraint_Fixed Int
                | Constraint_Min Int
  deriving (Show, Read, Eq, Ord)

-- | Compute the size of each widget "@k@" based on the total set of 'Constraint's
computeSizes
  :: Ord k
  => Int
  -> Map k (a, Constraint)
  -> Map k (a, Int)
computeSizes available constraints =
  let minTotal = sum $ ffor (Map.elems constraints) $ \case
        (_, Constraint_Fixed n) -> n
        (_, Constraint_Min n) -> n
      leftover = max 0 (available - minTotal)
      numStretch = Map.size $ Map.filter (isMin . snd) constraints
      szStretch = floor $ leftover % (max numStretch 1)
      adjustment = max 0 $ available - minTotal - szStretch * numStretch
  in snd $ Map.mapAccum (\adj (a, c) -> case c of
      Constraint_Fixed n -> (adj, (a, n))
      Constraint_Min n   -> (0, (a, n + szStretch + adj))) adjustment constraints
  where
    isMin (Constraint_Min _) = True
    isMin _                  = False

computeEdges :: (Ord k) => Map k (a, Int) -> Map k (a, (Int, Int))
computeEdges = fst . Map.foldlWithKey' (\(m, offset) k (a, sz) ->
  (Map.insert k (a, (offset, sz)) m, sz + offset)) (Map.empty, 0)








-- TODO should prob be (Branch [LayoutDebugTree] | Leaf PosDim
-- but it's weird cuz a leaf node won't know it's PosDim until combined with a Region...
data LayoutDebugTree t = LayoutDebugTree_Branch [LayoutDebugTree t] | LayoutDebugTree_Leaf

emptyLayoutDebugTree :: LayoutDebugTree t
emptyLayoutDebugTree = LayoutDebugTree_Leaf

class IsLayoutReturn t b a where
  getLayoutResult :: b -> a
  getLayoutNumChildren :: b -> Int
  getLayoutFocussedDyn :: b -> Dynamic t (Maybe Int)
  getLayoutTree :: b -> LayoutDebugTree t


instance IsLayoutReturn t (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a) a where
  getLayoutResult (_,_,_,a) = a
  getLayoutNumChildren (_,_,d,_) = d
  getLayoutFocussedDyn (_,d,_,_) = d
  getLayoutTree (tree,_,_,_) = tree

instance Reflex t => IsLayoutReturn t a a where
  getLayoutResult = id
  getLayoutNumChildren _ = 1
  getLayoutFocussedDyn _ = constDyn Nothing
  getLayoutTree _ = emptyLayoutDebugTree

class IsLayoutVtyWidget l t (m :: * -> *) where
  runIsLayoutVtyWidget :: l t m a -> Event t (Maybe Int) -> VtyWidget t m a

newtype LayoutVtyWidget t m a = LayoutVtyWidget {
    unLayoutVtyWidget :: ReaderT (Event t (Maybe Int)) (VtyWidget t m) a
  } deriving
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
    , MonadNodeId
    , PostBuild t
    )

instance MonadTrans (LayoutVtyWidget t) where
  lift x = LayoutVtyWidget $ lift $ lift x

instance IsLayoutVtyWidget VtyWidget t m where
  runIsLayoutVtyWidget w _ = w

instance IsLayoutVtyWidget LayoutVtyWidget t m where
  runIsLayoutVtyWidget = runReaderT . unLayoutVtyWidget
